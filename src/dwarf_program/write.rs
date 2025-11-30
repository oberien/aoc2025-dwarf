use std::io::Write;
use gimli::LittleEndian;
use gimli::write::{AttributeValue, EndianVec, RelocateWriter, Relocation, RelocationTarget, Sections, Writer};
use object::{RelocationEncoding, RelocationFlags, RelocationKind, SectionKind, SymbolFlags, SymbolKind, SymbolScope};
use object::write::{Object, SectionId, Symbol, SymbolSection};
use super::DwarfProgram;

/// Record information needed to write a section.
#[derive(Clone)]
pub(super) struct SectionWriter {
    pub(super) data: EndianVec<LittleEndian>,
    pub(super) relocations: Vec<Relocation>,
    pub(super) id: Option<SectionId>,
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


impl DwarfProgram {
    pub(super) fn write_sections(&mut self) -> gimli::write::Result<Sections<SectionWriter>> {
        let mut sections = Sections::new(SectionWriter::new());
        self.dwarf.write(&mut sections)?;
        Ok(sections)
    }

    pub fn write_to(mut self, writer: impl Write) -> Result<(), Box<dyn std::error::Error>> {
        let mut sections = self.write_sections()?;


        let mut obj = Object::new(
            object::BinaryFormat::native_object(),
            object::Architecture::X86_64,
            object::Endianness::Little,
        );

        let root = self.dwarf.unit.get_mut(self.dwarf.unit.root());
        root.set(gimli::DW_AT_name, AttributeValue::String(env!("CARGO_PKG_NAME").into()));
        root.set(
            gimli::DW_AT_language,
            AttributeValue::Language(gimli::DW_LANG_C11),
        );

        let rodata_sectionid = obj.add_section(Vec::new(), ".rodata".into(), SectionKind::ReadOnlyData);
        let mut rodata_symbols = Vec::new();
        for rodata in self.rodata {
            let offset = obj.append_section_data(rodata_sectionid, &rodata.data, 8);
            let symbol_id = obj.add_symbol(Symbol {
                name: rodata.name.into_bytes(),
                value: offset,
                size: rodata.data.len() as u64,
                kind: SymbolKind::Data,
                scope: SymbolScope::Linkage,
                weak: false,
                section: SymbolSection::Section(rodata_sectionid),
                flags: SymbolFlags::None,
            });
            rodata_symbols.push(symbol_id);
        }



        sections.for_each_mut(|id, section| -> object::write::Result<()> {
            if section.data.len() == 0 {
                return Ok(());
            }
            let kind = if id.is_string() { SectionKind::DebugString } else { SectionKind::Debug };
            let section_id = obj.add_section(Vec::new(), id.name().into(), kind);
            obj.set_section_data(section_id, section.data.take(), 1);

            section.id = Some(section_id);
            Ok(())
        })?;

        // Add the relocations to the object file.
        sections.for_each(|_, section| -> object::write::Result<()> {
            let Some(section_id) = section.id else {
                assert!(section.relocations.is_empty());
                return Ok(());
            };
            for reloc in &section.relocations {
                assert!(reloc.eh_pe.is_none());
                let symbol = match reloc.target {
                    RelocationTarget::Section(id) => {
                        let symbol = obj.section_symbol(sections.get(id).unwrap().id.unwrap());
                        symbol
                    }
                    RelocationTarget::Symbol(id) => rodata_symbols[id],
                };
                obj.add_relocation(
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

        obj.write_stream(writer)?;
        Ok(())
    }
}