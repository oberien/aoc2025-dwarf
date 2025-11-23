use gimli::write::{AttributeValue, DwarfUnit, Expression, UnitEntryId};
use gimli::{Encoding, Format};

mod run;
mod write;

struct RodataData {
    name: String,
    data: Vec<u8>,
}

struct BaseTypes {
    u64_typ: UnitEntryId,
    #[allow(unused)]
    u2048_typ: UnitEntryId,
}

pub struct DwarfProgram {
    dwarf: DwarfUnit,
    rodata: Vec<RodataData>,
    base_types: BaseTypes,
}

impl DwarfProgram {
    pub fn new() -> Self {
        let mut dwarf = DwarfUnit::new(Encoding {
            format: Format::Dwarf64,
            version: 5,
            address_size: 8,
        });

        let base_types = add_base_types(&mut dwarf);

        Self {
            dwarf,
            rodata: Vec::new(),
            base_types,
        }
    }

    pub fn add_rodata_data(&mut self, name: impl Into<String>, data: Vec<u8>) {
        self.rodata.push(RodataData { name: name.into(), data })
    }
    
    pub fn rodata_data_index(&self, name: &str) -> usize {
        self.rodata.iter().position(|s| s.name == name)
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


