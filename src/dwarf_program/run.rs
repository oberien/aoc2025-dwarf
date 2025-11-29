use std::collections::HashMap;
use gimli::{DW_AT_byte_size, DW_AT_encoding, DW_AT_location, DW_AT_name, DieReference, DwAte, Dwarf, EndianSlice, EvaluationResult, Expression, LittleEndian, Location, Piece, Reader, Unit, UnitOffset, UnitRef, Value, ValueType};
use gimli::write::{RelocateWriter, Relocation, RelocationTarget, Sections, Writer};
use crate::dwarf_program::{DwarfProgram, RodataData};
use crate::dwarf_program::write::SectionWriter;

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


        relocate(&mut sections, &rodata);

        let dwarf = Dwarf::load(|sectionid| Ok::<_, ()>(sections.get(sectionid)
            .map(|endian_vec| EndianSlice::new(endian_vec.writer().slice(), LittleEndian))
            .unwrap_or_default())
        ).unwrap();

        let root = root_unit(&dwarf);
        let root = root.unit_ref(&dwarf);
        let expr = find_expression_of_die(&root, &die_name)
            .unwrap_or_else(|| panic!("DIE {die_name} not found"));

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
                    if rel == 0 {
                        // handle __debug_stack
                        println!("{:#?}", &evaluation.stack());
                    }
                    evaluation.resume_with_relocated_address(rel)
                },
                Ok(EvaluationResult::RequiresIndexedAddress { .. }) => panic!("indexed address unsupported"),
                Ok(EvaluationResult::RequiresBaseType(offset)) => {
                    let size = root.entry(offset).unwrap().attr(DW_AT_byte_size).unwrap().unwrap().udata_value().unwrap();
                    let encoding = DwAte(root.entry(offset).unwrap().attr(DW_AT_encoding).unwrap().unwrap().u8_value().unwrap());
                    evaluation.resume_with_base_type(ValueType::from_encoding(encoding, size).unwrap())
                },
                Err(e) => panic!("error evaluating expression: {e:?}"),
            };
        }

        let res = evaluation.result();
        assert_eq!(res.len(), 1);
        let Piece { size_in_bits, bit_offset, location } = res[0];
        assert_eq!(size_in_bits, None);
        assert_eq!(bit_offset, None);
        let Location::Value { value } = location else { panic!("invalid location {location:?}") };
        value
    }
}

fn relocate(sections: &mut Sections<SectionWriter>, rodata: &Rodata) {
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
}

fn root_unit<R: Reader>(dwarf: &Dwarf<R>) -> Unit<R> {
    let mut iter = dwarf.units();
    let root = iter.next().unwrap().unwrap();
    assert!(iter.next().unwrap().is_none());
    dwarf.unit(root).unwrap()
}

fn find_expression_of_die<'a>(root: &UnitRef<EndianSlice<'a, LittleEndian>>, die_name: &str) -> Option<Expression<EndianSlice<'a, LittleEndian>>> {
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
        return Some(expr);
    }
    None
}
