use std::collections::HashMap;
use gimli::DW_OP_drop;
use gimli::write::{Address, Expression, UnitEntryId};
use crate::dwarf_program::DwarfProgram;
use crate::parse::{DefineData, Instruction, Item};

struct Functions<'a> {
    procedures: HashMap<&'a str, UnitEntryId>,
    variables: HashMap<&'a str, UnitEntryId>,
}
struct FunctionContext<'a> {
    control_flow_targets: Vec<(usize, &'a str)>,
    label_locations: HashMap<&'a str, usize>,
}

pub fn compile(program: &mut DwarfProgram, items: Vec<Item<'_>>) {
    let functions = first_pass(program, &items);
    second_pass(program, &functions, items);
}

fn first_pass<'a>(program: &mut DwarfProgram, items: &Vec<Item<'a>>) -> Functions<'a> {
    let mut context = Functions {
        procedures: HashMap::new(),
        variables: HashMap::new(),
    };
    for item in items {
        match item {
            Item::Rodata { .. } => (),
            Item::Procedure { name, body: _ } => {
                let unit = program.add_dwarf_procedure(name.to_string());
                context.procedures.insert(name, unit);
            }
            Item::Variable { name, body: _ } => {
                let unit = program.add_dwarf_variable(name.to_string());
                context.variables.insert(name, unit);
            }
        }
    }
    context
}

fn second_pass<'a>(program: &mut DwarfProgram, functions: &Functions<'a>, items: Vec<Item<'a>>) {
    for item in items {
        match item {
            Item::Rodata { name, def_data } => {
                let mut data = Vec::new();
                for def in def_data {
                    match def {
                        DefineData::U8(u) => data.extend(u.into_iter().map(|u| u.number)),
                        DefineData::U16(u) => data.extend(u.into_iter().flat_map(|u| u.number.to_ne_bytes())),
                        DefineData::U32(u) => data.extend(u.into_iter().flat_map(|u| u.number.to_ne_bytes())),
                        DefineData::U64(u) => data.extend(u.into_iter().flat_map(|u| u.number.to_ne_bytes())),
                        DefineData::IncludeFile(path) => data.extend(std::fs::read(&*path).unwrap()),
                    }
                }
                let len_name = format!("{name}.len");
                program.add_rodata_data(len_name.clone(), data.len().to_ne_bytes().to_vec());
                program.add_rodata_data(name.to_owned(), data);
            }
            Item::Procedure { name, body } => {
                let mut context = FunctionContext {
                    control_flow_targets: Vec::new(),
                    label_locations: HashMap::new(),
                };
                let mut expr = compile_function(program, functions, &mut context, body);
                for (operation, label) in context.control_flow_targets {
                    expr.set_target(operation, context.label_locations[label]);
                }
                let unit = functions.procedures[name];
                program.set_expression(unit, expr);
            },
            Item::Variable { name, body } => {
                let mut context = FunctionContext {
                    control_flow_targets: Vec::new(),
                    label_locations: HashMap::new(),
                };
                let mut expr = compile_function(program, functions, &mut context, body);
                expr.op(gimli::DW_OP_stack_value);
                for (operation, label) in context.control_flow_targets {
                    expr.set_target(operation, context.label_locations[label]);
                }
                let unit = functions.variables[name];
                program.set_expression(unit, expr);
            },
        }
    }
}

fn compile_function<'a>(program: &mut DwarfProgram, functions: &Functions<'a>, context: &mut FunctionContext<'a>, instructions: Vec<Instruction<'a>>) -> Expression {
    let mut expr = Expression::new();
    for instruction in instructions {
        match instruction {
            Instruction::Addr(name, addend) => {
                let data_index = program.rodata_data_index(name);
                expr.op_addr(Address::Symbol { symbol: data_index, addend })
            },
            Instruction::Constu(unsigned) => expr.op_constu(unsigned.number),
            Instruction::Consts(signed) => expr.op_consts(signed.number),
            // Instruction::ConstType(_) => todo!(),
            Instruction::Dup => expr.op(gimli::DW_OP_dup),
            Instruction::Drop => expr.op(gimli::DW_OP_drop),
            Instruction::Pick(index) => expr.op_pick(index.number),
            Instruction::Swap => expr.op(gimli::DW_OP_swap),
            Instruction::Rot => expr.op(gimli::DW_OP_rot),
            Instruction::Deref => expr.op_deref(),
            Instruction::DerefSize(size) => expr.op_deref_size(size.number),
            Instruction::Abs => expr.op(gimli::DW_OP_abs),
            Instruction::And => expr.op(gimli::DW_OP_and),
            Instruction::Div => expr.op(gimli::DW_OP_div),
            Instruction::Minus => expr.op(gimli::DW_OP_minus),
            Instruction::Mod => expr.op(gimli::DW_OP_mod),
            Instruction::Mul => expr.op(gimli::DW_OP_mul),
            Instruction::Neg => expr.op(gimli::DW_OP_neg),
            Instruction::Not => expr.op(gimli::DW_OP_not),
            Instruction::Or => expr.op(gimli::DW_OP_or),
            Instruction::Plus => expr.op(gimli::DW_OP_plus),
            Instruction::PlusUconst(unsigned) => expr.op_plus_uconst(unsigned.number),
            Instruction::Shl => expr.op(gimli::DW_OP_shl),
            Instruction::Shr => expr.op(gimli::DW_OP_shr),
            Instruction::Shra => expr.op(gimli::DW_OP_shra),
            Instruction::Xor => expr.op(gimli::DW_OP_xor),
            Instruction::Le => expr.op(gimli::DW_OP_le),
            Instruction::Ge => expr.op(gimli::DW_OP_ge),
            Instruction::Eq => expr.op(gimli::DW_OP_eq),
            Instruction::Lt => expr.op(gimli::DW_OP_lt),
            Instruction::Gt => expr.op(gimli::DW_OP_gt),
            Instruction::Ne => expr.op(gimli::DW_OP_ne),
            Instruction::Skip(label) => context.control_flow_targets.push(( expr.op_skip(), label)),
            Instruction::Bra(label) => context.control_flow_targets.push(( expr.op_bra(), label)),
            Instruction::Call(name) => expr.op_call(functions.procedures[name]),
            Instruction::Nop => expr.op(gimli::DW_OP_nop),
            Instruction::Label(label) => assert!(context.label_locations.insert(label, expr.next_index()).is_none()),
            Instruction::Debug => {
                let data_index = program.rodata_data_index("__debug_stack");
                expr.op_addr(Address::Symbol { symbol: data_index, addend: 0 });
                expr.op(DW_OP_drop);
            }
        }
    }
    expr
}