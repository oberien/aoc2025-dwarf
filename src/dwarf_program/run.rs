use std::collections::HashMap;
use gimli::{DW_AT_location, DW_AT_name, DieReference, Dwarf, EndianSlice, EvaluationResult, LittleEndian, Location, Piece, UnitOffset, Value};
use gimli::write::{RelocateWriter, Relocation, RelocationTarget, Writer};
use crate::dwarf_program::{DwarfProgram, RodataData};

struct Rodata {
    data: Vec<u8>,
    relocations: HashMap<usize, usize>,
}
impl Rodata {
    fn new() -> Self {
        Self {
            data: Vec::new(),
            relocations: HashMap::new(),
        }
    }
    fn add_rodata(&mut self, data_index: usize, data: &[u8]) {
        self.relocations.insert(data_index, self.data.len());
        self.data.extend_from_slice(data);
    }
    fn relocate(&self, data_index: usize) -> usize {
        self.relocations[&data_index]
    }
}

impl DwarfProgram {
    pub fn run(mut self, die_name: String) -> Value {
        let mut sections = self.write_sections().unwrap();

        let mut rodata = Rodata::new();
        for (i, RodataData { name: _, data }) in self.rodata.into_iter().enumerate() {
            rodata.add_rodata(i, &data);
        }


        // relocate
        sections.for_each_mut(|_, section| -> Result<(), ()> {
            for &Relocation { offset, size, target, addend, eh_pe } in &section.relocations {
                assert!(eh_pe.is_none());
                let symbol = match target {
                    RelocationTarget::Section(id) => {
                        eprintln!("not relocating {id:?}");
                        continue
                    },
                    RelocationTarget::Symbol(id) => id,
                };
                let addr = (rodata.relocate(symbol) as i64 + addend).try_into().unwrap();
                section.data.write_udata_at(offset, addr, size).unwrap();
            }
            Ok(())
        }).unwrap();


        let dwarf = Dwarf::load(|sectionid| Ok::<_, ()>(sections.get(sectionid)
                .map(|endian_vec| EndianSlice::new(endian_vec.writer().slice(), LittleEndian))
            .unwrap_or_default())
        ).unwrap();
        let mut iter = dwarf.units();
        let root = iter.next().unwrap().unwrap();
        assert!(iter.next().unwrap().is_none());
        let root = dwarf.unit(root).unwrap();
        let root = root.unit_ref(&dwarf);
        let mut entries = root.entries();
        while let Ok(Some((_, entry))) = entries.next_dfs() {
            let Ok(Some(name)) = entry.attr_value(DW_AT_name) else { continue };
            let Ok(name) = root.attr_string(name) else { continue };
            if name.to_string_lossy() != die_name {
                continue;
            }
            let Ok(Some(loc)) = entry.attr_value(DW_AT_location) else { continue };
            let expr = loc.exprloc_value()
                .unwrap_or_else(|| panic!("found DIE {die_name} but location isn't a DWARF expression: {loc:?}"));
            let mut evaluation = expr.evaluation(root.encoding());
            let mut res = evaluation.evaluate();
            loop {
                res = match res {
                    Ok(EvaluationResult::Complete) => break,
                    Ok(EvaluationResult::RequiresMemory { address, size, space, base_type }) => {
                        assert_eq!(size, 8);
                        assert!(space.is_none());
                        assert_eq!(base_type, UnitOffset(0));
                        println!("requires memory {address} {size} {space:?} {base_type:?}");
                        let val = u64::from_le_bytes(rodata.data[address as usize..][..size as usize].try_into().unwrap());
                        evaluation.resume_with_memory(Value::Generic(val))
                    },
                    Ok(EvaluationResult::RequiresRegister { .. }) => panic!("register unsupported"),
                    Ok(EvaluationResult::RequiresFrameBase) => panic!("frame base unsupported"),
                    Ok(EvaluationResult::RequiresTls(_)) => panic!("thread-local storage unsupported"),
                    Ok(EvaluationResult::RequiresCallFrameCfa) => panic!("cfa unsupported"),
                    Ok(EvaluationResult::RequiresAtLocation(die_ref)) => {
                        println!("die ref {die_ref:?}");
                        let slice = match die_ref {
                            DieReference::UnitRef(offset) => root.entry(offset).unwrap().attr(DW_AT_location).unwrap().unwrap().exprloc_value().unwrap().0,
                            DieReference::DebugInfoRef(_) => unimplemented!("DebugInfoRef into another unit"),
                        };
                        evaluation.resume_with_at_location(slice)
                    },
                    Ok(EvaluationResult::RequiresEntryValue(_)) => panic!("entry value unsupported"),
                    Ok(EvaluationResult::RequiresParameterRef(_)) => panic!("parameter ref unsupported"),
                    Ok(EvaluationResult::RequiresRelocatedAddress(rel)) => {
                        evaluation.resume_with_relocated_address(rel)
                    },
                    Ok(EvaluationResult::RequiresIndexedAddress { .. }) => panic!("indexed address unsupported"),
                    Ok(EvaluationResult::RequiresBaseType(_)) => panic!("base type unsupported"),
                    Err(e) => panic!("error evaluating expression: {e:?}"),
                };
            }
            let res = evaluation.result();
            assert_eq!(res.len(), 1);
            let Piece { size_in_bits, bit_offset, location } = res[0];
            assert_eq!(size_in_bits, None);
            assert_eq!(bit_offset, None);
            let Location::Value { value } = location else { panic!("invalid location {location:?}") };
            return value;
        }
        panic!("DIE {die_name} not found");
    }
}