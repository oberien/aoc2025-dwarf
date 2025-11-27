use gimli::write::{AttributeValue, DwarfUnit, Expression, UnitEntryId};
use gimli::{DwAte, Encoding, Format};

mod run;
mod write;

struct RodataData {
    name: String,
    data: Vec<u8>,
}

pub struct DwarfProgram {
    dwarf: DwarfUnit,
    rodata: Vec<RodataData>,
}

impl DwarfProgram {
    pub fn new() -> Self {
        let dwarf = DwarfUnit::new(Encoding {
            format: Format::Dwarf64,
            version: 5,
            address_size: 8,
        });

        Self {
            dwarf,
            rodata: vec![RodataData { name: "__debug_stack".into(), data: vec![0u8; 8] }],
        }
    }

    pub fn create_base_type(&mut self, name: impl Into<String>, kind: DwAte) -> UnitEntryId {
        let entry_id = self.dwarf.unit.add(self.dwarf.unit.root(), gimli::DW_TAG_base_type);
        let entry = self.dwarf.unit.get_mut(entry_id);
        entry.set(gimli::DW_AT_encoding, AttributeValue::Encoding(kind));
        entry.set(gimli::DW_AT_name, AttributeValue::String(name.into().into()));
        entry_id
    }
    pub fn set_base_type_size(&mut self, type_die: UnitEntryId, size: u8) {
        self.dwarf.unit.get_mut(type_die).set(gimli::DW_AT_byte_size, AttributeValue::Data1(size));
    }
    pub fn add_base_type(&mut self, name: impl Into<String>, size: u8, kind: DwAte) -> UnitEntryId {
        let entry = self.create_base_type(name, kind);
        self.set_base_type_size(entry, size);
        entry
    }

    pub fn add_rodata_data(&mut self, name: impl Into<String>, data: Vec<u8>) {
        self.rodata.push(RodataData { name: name.into(), data })
    }
    
    pub fn rodata_data_index(&self, name: &str) -> usize {
        self.rodata.iter().position(|s| s.name == name)
            .unwrap_or_else(|| panic!("unknown .rodata data `{name}`"))
    }

    pub fn add_dwarf_procedure(&mut self, name: String) -> UnitEntryId {
        let procedure = self.dwarf.unit.add(self.dwarf.unit.root(), gimli::DW_TAG_dwarf_procedure);
        let entry = self.dwarf.unit.get_mut(procedure);
        entry.set(gimli::DW_AT_name, AttributeValue::String(format!("procedure.{name}").into_bytes()));
        procedure
    }
    pub fn add_dwarf_variable(&mut self, name: String) -> UnitEntryId {
        let variable = self.dwarf.unit.add(self.dwarf.unit.root(), gimli::DW_TAG_variable);
        let entry = self.dwarf.unit.get_mut(variable);
        entry.set(gimli::DW_AT_name, AttributeValue::String(format!("{name}").into_bytes()));
        entry.set(gimli::DW_AT_external, AttributeValue::Flag(true));
        variable
    }
    pub fn set_expression(&mut self, entry: UnitEntryId, expr: Expression) {
        self.dwarf.unit.get_mut(entry).set(gimli::DW_AT_location, AttributeValue::Exprloc(expr))
    }
    pub fn set_type(&mut self, entry: UnitEntryId, type_die: UnitEntryId) {
        self.dwarf.unit.get_mut(entry).set(gimli::DW_AT_type, AttributeValue::UnitRef(type_die));
    }
}
