//! A small example for writing an object file containing DWARF sections.
//!
//! The resulting object file can be linked with a C runtime to create a complete executable:
//! ```sh
//! $ cargo run --bin simple_write
//! $ gcc -o hello hello.o -z noexecstack
//! $ ./hello
//! Hello, world!
//! ```

use std::fs::File;
use std::path::Path;
use gimli::write::{AttributeValue, DwarfUnit, EndianVec, Expression, RelocateWriter, Relocation, RelocationTarget, Sections, UnitEntryId, Writer};
use gimli::{Encoding, Format, LittleEndian};
use object::{RelocationEncoding, RelocationFlags, RelocationKind, SectionKind, SymbolFlags, SymbolKind, SymbolScope};
use object::write::{Object, SectionId, Symbol, SymbolId, SymbolSection};

/// Record information needed to write a section.
#[derive(Clone)]
struct SectionWriter {
    data: EndianVec<LittleEndian>,
    relocations: Vec<Relocation>,
    id: Option<SectionId>,
}

impl SectionWriter {
    fn new() -> Self {
        Self {
            data: EndianVec::new(LittleEndian),
            relocations: Vec::new(),
            id: None,
        }
    }
}

impl RelocateWriter for SectionWriter {
    type Writer = EndianVec<LittleEndian>;

    fn writer(&self) -> &Self::Writer {
        &self.data
    }

    fn writer_mut(&mut self) -> &mut Self::Writer {
        &mut self.data
    }

    fn relocate(&mut self, relocation: Relocation) {
        self.relocations.push(relocation);
    }
}

struct RodataData {
    name: String,
    symbol_id: SymbolId,
}

struct BaseTypes {
    u64_typ: UnitEntryId,
    #[allow(unused)]
    u2048_typ: UnitEntryId,
}

pub struct DwarfProgram {
    obj: Object<'static>,
    dwarf: DwarfUnit,
    rodata_sectionid: SectionId,
    rodata_symbols: Vec<RodataData>,
    base_types: BaseTypes,
}

impl DwarfProgram {
    pub fn new() -> Self {
        let mut dwarf = DwarfUnit::new(Encoding {
            format: Format::Dwarf64,
            version: 5,
            address_size: 8,
        });
        let root = dwarf.unit.get_mut(dwarf.unit.root());
        root.set(gimli::DW_AT_name, AttributeValue::String("aoc2025-dwarf".into()));
        root.set(
            gimli::DW_AT_language,
            AttributeValue::Language(gimli::DW_LANG_C11),
        );
        
        let base_types = add_base_types(&mut dwarf);

        let mut obj = Object::new(
            object::BinaryFormat::native_object(),
            object::Architecture::X86_64,
            object::Endianness::Little,
        );
        let rodata_sectionid = obj.add_section(Vec::new(), ".rodata".into(), SectionKind::ReadOnlyData);
        Self {
            obj,
            dwarf,
            rodata_sectionid,
            rodata_symbols: Vec::new(),
            base_types,
        }
    }

    pub fn add_rodata_data(&mut self, name: impl Into<String>, content: &[u8]) {
        let name = name.into();
        let offset = self.obj.append_section_data(self.rodata_sectionid, content, 8);
        let symbol_id = self.obj.add_symbol(Symbol {
            name: name.clone().into_bytes(),
            value: offset,
            size: content.len() as u64,
            kind: SymbolKind::Data,
            scope: SymbolScope::Linkage,
            weak: false,
            section: SymbolSection::Section(self.rodata_sectionid),
            flags: SymbolFlags::None,
        });
        self.rodata_symbols.push(RodataData {
            name,
            symbol_id,
        });
    }
    
    pub fn rodata_symbol(&self, name: &str) -> usize {
        self.rodata_symbols.iter().position(|s| s.name == name)
            .unwrap_or_else(|| panic!("unknown .rodata data `{name}`"))
    }

    pub fn add_dwarf_variable(&mut self, name: String) -> UnitEntryId {
        let variable = self.dwarf.unit.add(self.dwarf.unit.root(), gimli::DW_TAG_variable);
        let entry = self.dwarf.unit.get_mut(variable);
        entry.set(gimli::DW_AT_name, AttributeValue::String(name.into_bytes()));
        entry.set(gimli::DW_AT_external, AttributeValue::Flag(true));
        entry.set(gimli::DW_AT_type, AttributeValue::UnitRef(self.base_types.u64_typ));
        variable
    }
    pub fn set_variable_expression(&mut self, entry: UnitEntryId, expr: Expression) {
        self.dwarf.unit.get_mut(entry).set(gimli::DW_AT_location, AttributeValue::Exprloc(expr))
    }
    
    pub fn write_to_file(mut self, path: impl AsRef<Path>) -> Result<(), Box<dyn std::error::Error>> {
        let mut sections = Sections::new(SectionWriter::new());
        self.dwarf.write(&mut sections)?;


        sections.for_each_mut(|id, section| -> object::write::Result<()> {
            if section.data.len() == 0 {
                return Ok(());
            }
            let kind = if id.is_string() { SectionKind::DebugString } else { SectionKind::Debug };
            let section_id = self.obj.add_section(Vec::new(), id.name().into(), kind);
            self.obj.set_section_data(section_id, section.data.take(), 1);

            section.id = Some(section_id);
            Ok(())
        })?;

        // Add the relocations to the object file.
        sections.for_each(|_, section| -> object::write::Result<()> {
            let Some(section_id) = section.id else {
                debug_assert!(section.relocations.is_empty());
                return Ok(());
            };
            for reloc in &section.relocations {
                // The `eh_pe` field is not used in this example because we are not writing
                // unwind information.
                debug_assert!(reloc.eh_pe.is_none());
                let symbol = match reloc.target {
                    RelocationTarget::Section(id) => {
                        let symbol = self.obj.section_symbol(sections.get(id).unwrap().id.unwrap());
                        symbol
                    }
                    RelocationTarget::Symbol(id) => self.rodata_symbols[id].symbol_id,
                };
                self.obj.add_relocation(
                    section_id,
                    object::write::Relocation {
                        offset: reloc.offset as u64,
                        symbol,
                        addend: reloc.addend,
                        flags: RelocationFlags::Generic {
                            kind: RelocationKind::Absolute,
                            encoding: RelocationEncoding::Generic,
                            size: reloc.size * 8,
                        },
                    },
                )?;
            }
            Ok(())
        })?;

        let file = File::create(path)?;
        self.obj.write_stream(file)?;
        Ok(())
    }
}

fn add_base_types(dwarf: &mut DwarfUnit) -> BaseTypes {
    let u64_typ = dwarf.unit.add(dwarf.unit.root(), gimli::DW_TAG_base_type);
    let entry = dwarf.unit.get_mut(u64_typ);
    entry.set(gimli::DW_AT_byte_size, AttributeValue::Data1(8));
    entry.set(gimli::DW_AT_encoding, AttributeValue::Encoding(gimli::DW_ATE_unsigned));
    entry.set(gimli::DW_AT_name, AttributeValue::String("u64".into()));

    let u2048_typ = dwarf.unit.add(dwarf.unit.root(), gimli::DW_TAG_base_type);
    let entry = dwarf.unit.get_mut(u2048_typ);
    entry.set(gimli::DW_AT_byte_size, AttributeValue::Data1(255));
    entry.set(gimli::DW_AT_encoding, AttributeValue::Encoding(gimli::DW_ATE_unsigned));
    entry.set(gimli::DW_AT_name, AttributeValue::String("u2048".into()));
    
    BaseTypes { u64_typ, u2048_typ }
}


