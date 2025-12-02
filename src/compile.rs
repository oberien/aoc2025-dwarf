use std::collections::{HashMap, VecDeque};
use gimli::write::{Address, Expression, UnitEntryId};
use itertools::Itertools;
use crate::dwarf_program::DwarfProgram;
use crate::parse::{Addr, CondOp, CustomType, CustomTypeInit, DefineData, Instruction, Item, Path, Primitive, Type, TypeInit, TypeOrGeneric, U64};

struct GlobalContext<'a> {
    procedures: HashMap<&'a str, UnitEntryId>,
    variables: HashMap<&'a str, UnitEntryId>,
    custom_types: HashMap<&'a str, CustomType<'a>>,
    type_dies: HashMap<Type<'a>, UnitEntryId>,
    anonymous_label_number: u64,
}
impl GlobalContext<'_> {
    fn anonymous_label(&mut self) -> String {
        self.anonymous_label_number += 1;
        format!("._anonymous_{}", self.anonymous_label_number)
    }
}
struct FunctionContext {
    control_flow_targets: Vec<(usize, String)>,
    label_locations: HashMap<String, usize>,
}

pub fn compile(program: &mut DwarfProgram, items: Vec<Item<'_>>) {
    let mut global_ctx = first_pass(program, &items);
    second_pass(program, &mut global_ctx, items);
}

fn first_pass<'a>(program: &mut DwarfProgram, items: &Vec<Item<'a>>) -> GlobalContext<'a> {
    let mut global_ctx = GlobalContext {
        procedures: HashMap::new(),
        variables: HashMap::new(),
        custom_types: HashMap::new(),
        type_dies: HashMap::new(),
        anonymous_label_number: 0,
    };
    global_ctx.type_dies.insert(Type::Primitive(Primitive::U8), program.add_base_type("u8", 1, gimli::DW_ATE_unsigned));
    global_ctx.type_dies.insert(Type::Primitive(Primitive::U16), program.add_base_type("u16", 2, gimli::DW_ATE_unsigned));
    global_ctx.type_dies.insert(Type::Primitive(Primitive::U32), program.add_base_type("u32", 4, gimli::DW_ATE_unsigned));
    global_ctx.type_dies.insert(Type::Primitive(Primitive::U64), program.add_base_type("u64", 8, gimli::DW_ATE_unsigned));
    global_ctx.type_dies.insert(Type::Primitive(Primitive::I8), program.add_base_type("i8", 1, gimli::DW_ATE_signed));
    global_ctx.type_dies.insert(Type::Primitive(Primitive::I16), program.add_base_type("i16", 2, gimli::DW_ATE_signed));
    global_ctx.type_dies.insert(Type::Primitive(Primitive::I32), program.add_base_type("i32", 4, gimli::DW_ATE_signed));
    global_ctx.type_dies.insert(Type::Primitive(Primitive::I64), program.add_base_type("i64", 8, gimli::DW_ATE_signed));
    global_ctx.type_dies.insert(Type::Primitive(Primitive::F32), program.add_base_type("f32", 4, gimli::DW_ATE_float));
    global_ctx.type_dies.insert(Type::Primitive(Primitive::F64), program.add_base_type("f64", 8, gimli::DW_ATE_float));
    for item in items {
        match item {
            Item::Rodata { .. } => (),
            Item::Type(custom_type) => {
                assert!(global_ctx.custom_types.insert(custom_type.name, custom_type.clone()).is_none());
                let entry = program.create_base_type(custom_type.name.to_string(), gimli::DW_ATE_unsigned);
                assert!(global_ctx.type_dies.insert(Type::Custom(custom_type.name), entry).is_none());
            },
            Item::Procedure { name, body: _ } => {
                let unit = program.add_dwarf_procedure(name.to_string());
                global_ctx.procedures.insert(name, unit);
            }
            Item::Variable { name, body: _ } => {
                let unit = program.add_dwarf_variable(name.to_string());
                global_ctx.variables.insert(name, unit);
            }
        }
    }
    global_ctx
}

fn second_pass<'a>(program: &mut DwarfProgram, global_ctx: &mut GlobalContext<'a>, items: Vec<Item<'a>>) {
    for item in items {
        match item {
            Item::Rodata { name, def_data } => {
                let data: Vec<u8> = def_data.iter().flat_map(DefineData::to_vec).collect();
                let len_name = format!("{name}.len");
                program.add_rodata_data(len_name.clone(), data.len().to_le_bytes().to_vec());
                program.add_rodata_data(name.to_owned(), data);
            }
            Item::Type(CustomType { name, fields: _ }) => {
                let size = type_size(Type::Custom(name), global_ctx);
                assert!(size <= 255, "type {name} is larger than 255 bytes");
                let type_die = global_ctx.type_dies[&Type::Custom(name)];
                program.set_base_type_size(type_die, size as u8);
            }
            Item::Procedure { name, body } => {
                let mut context = FunctionContext {
                    control_flow_targets: Vec::new(),
                    label_locations: HashMap::new(),
                };
                let mut expr = compile_function(program, global_ctx, &mut context, body);
                for (operation, label) in context.control_flow_targets {
                    expr.set_target(operation, context.label_locations[&label]);
                }
                let unit = global_ctx.procedures[name];
                program.set_expression(unit, expr);
            },
            Item::Variable { name, body } => {
                let mut fn_ctx = FunctionContext {
                    control_flow_targets: Vec::new(),
                    label_locations: HashMap::new(),
                };
                let mut expr = compile_function(program, global_ctx, &mut fn_ctx, body);
                expr.op(gimli::DW_OP_stack_value);
                for (operation, label) in fn_ctx.control_flow_targets {
                    expr.set_target(operation, fn_ctx.label_locations[&label]);
                }
                let unit = global_ctx.variables[name];
                program.set_expression(unit, expr);
                // program.set_type(unit, global_ctx.type_dies[&Type::Custom("$Foo")])
                program.set_type(unit, global_ctx.type_dies[&Type::Primitive(Primitive::U64)])
            },
        }
    }
}

fn compile_function<'a>(program: &mut DwarfProgram, global_ctx: &mut GlobalContext<'a>, fn_ctx: &mut FunctionContext, instructions: Vec<Instruction<'a>>) -> Expression {
    let mut expr = Expression::new();
    for instruction in instructions {
        compile_instruction(&mut expr, program, global_ctx, fn_ctx, instruction);
    }
    expr
}

fn compile_instruction<'a>(expr: &mut Expression, program: &mut DwarfProgram, global_ctx: &mut GlobalContext<'a>, fn_ctx: &mut FunctionContext, instruction: Instruction<'_>) {
    match instruction {
        Instruction::Addr(Addr::Addr(addr)) => {
            expr.op_addr(Address::Constant(addr.number));
        },
        Instruction::Addr(Addr::RodataRef(name, addend)) => {
            let data_index = program.rodata_data_index(name);
            expr.op_addr(Address::Symbol { symbol: data_index, addend });
        },
        Instruction::Constu(unsigned) => expr.op_constu(unsigned.number),
        Instruction::Consts(signed) => expr.op_consts(signed.number),
        Instruction::ConstType(typ, data) => {
            let data = data.to_vec();
            let type_die = global_ctx.type_dies[&typ];
            let size = type_size(typ, global_ctx);
            assert_eq!(
                data.len(), size,
                "const_type type's size differs from provided data: expected {size}, found {}", data.len()
            );
            expr.op_const_type(type_die, data.into_boxed_slice());
        },
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
        Instruction::Skip(label) => fn_ctx.control_flow_targets.push((expr.op_skip(), label.to_string())),
        Instruction::Bra(label) => fn_ctx.control_flow_targets.push((expr.op_bra(), label.to_string())),
        Instruction::Call(name) => expr.op_call(global_ctx.procedures[name]),
        Instruction::Convert(typ) => expr.op_convert(match typ {
            TypeOrGeneric::Generic => None,
            TypeOrGeneric::Type(typ) => Some(global_ctx.type_dies[&typ]),
        }),
        Instruction::Reinterpret(typ) => expr.op_reinterpret(match typ {
            TypeOrGeneric::Generic => None,
            TypeOrGeneric::Type(typ) => Some(global_ctx.type_dies[&typ]),
        }),
        Instruction::Nop => expr.op(gimli::DW_OP_nop),
        Instruction::Label(label) => assert!(fn_ctx.label_locations.insert(label.to_string(), expr.next_index()).is_none()),
        Instruction::Debug => {
            let data_index = program.rodata_data_index("__debug_stack");
            expr.op_addr(Address::Symbol { symbol: data_index, addend: 0 });
            expr.op(gimli::DW_OP_drop);
        },
        Instruction::IfElse(ifs, els) => {
            let end_label = global_ctx.anonymous_label();
            for (lhs, op, rhs, then) in ifs {
                // a < b needs to be translated as
                // compile(a)
                // compile(b)
                // ge ; OPPOSITE CONDITION
                // bra .next_if_or_else_or_after
                let next_label = global_ctx.anonymous_label();
                for lhs in lhs {
                    compile_instruction(expr, program, global_ctx, fn_ctx, lhs);
                }
                for rhs in rhs {
                    compile_instruction(expr, program, global_ctx, fn_ctx, rhs);
                }
                match op {
                    CondOp::Lt => expr.op(gimli::DW_OP_ge),
                    CondOp::Le => expr.op(gimli::DW_OP_gt),
                    CondOp::Eq => expr.op(gimli::DW_OP_ne),
                    CondOp::Ge => expr.op(gimli::DW_OP_lt),
                    CondOp::Gt => expr.op(gimli::DW_OP_le),
                    CondOp::Ne => expr.op(gimli::DW_OP_eq),
                }
                compile_instruction(expr, program, global_ctx, fn_ctx, Instruction::Bra(&next_label));
                for inst in then {
                    compile_instruction(expr, program, global_ctx, fn_ctx, inst);
                }
                compile_instruction(expr, program, global_ctx, fn_ctx, Instruction::Skip(&end_label));
                compile_instruction(expr, program, global_ctx, fn_ctx, Instruction::Label(&next_label));
            }
            if !els.is_empty() {
                for inst in els {
                    compile_instruction(expr, program, global_ctx, fn_ctx, inst);
                }
            }
            compile_instruction(expr, program, global_ctx, fn_ctx, Instruction::Label(&end_label));
        }
        Instruction::Create(custom_type_init) => {
            let type_die = global_ctx.type_dies[&Type::Custom(custom_type_init.name)];
            let mut vec = VecDeque::new();
            init_custom_type(custom_type_init, &mut vec, global_ctx);
            let data = Vec::from(vec).into_boxed_slice();
            expr.op_const_type(type_die, data);
        }
        Instruction::Set(Path { typ, path }) => {
            let (offset, primitive) = field_offset(typ, &path, global_ctx);
            let primitive_size = primitive_size(primitive);
            let type_size = type_size(Type::Custom(typ), global_ctx);
            let type_die = global_ctx.type_dies[&Type::Custom(typ)];
            let left_offset = type_size - primitive_size - offset as usize;
            // mask topmost stack element
            expr.op_constu(primitive_mask(primitive));
            expr.op(gimli::DW_OP_and);
            // convert topmost stack element to our type
            expr.op_convert(Some(type_die));
            // shift into correct position
            expr.op_const_type(type_die, int_to_type_array(left_offset as u64 * 8, type_size));
            expr.op(gimli::DW_OP_shl);
            // zero-out our "struct" by ANDing a mask
            expr.op(gimli::DW_OP_swap);
            let mut mask = vec![0xffu8; type_size];
            mask[left_offset..][..primitive_size].fill(0x00);
            expr.op_const_type(type_die, mask.into_boxed_slice());
            expr.op(gimli::DW_OP_and);
            // write field by ORing struct and value
            expr.op(gimli::DW_OP_or);
        }
        Instruction::Get(Path { typ, path }) => {
            let (offset, primitive) = field_offset(typ, &path, global_ctx);
            let primitive_size = primitive_size(primitive);
            let type_size = type_size(Type::Custom(typ), global_ctx);
            let left_offset = type_size as u8 - primitive_size as u8 - offset;
            let type_die = global_ctx.type_dies[&Type::Custom(typ)];
            expr.op_constu(left_offset as u64 * 8);
            expr.op_convert(Some(type_die));
            expr.op(gimli::DW_OP_shr);
            expr.op_convert(None);
            expr.op_constu(primitive_mask(primitive));
            expr.op(gimli::DW_OP_and);
        }
        Instruction::SetIndex(path) => {
            assert!(path.path.last().unwrap().1.is_none(), "#setindex's last field must not have an index: {path}");
            let Path { typ, mut path } = path;
            path.last_mut().unwrap().1 = Some(0);
            let (offset, primitive) = field_offset(typ, &path, global_ctx);
            let primitive_size = primitive_size(primitive);
            let type_size = type_size(Type::Custom(typ), global_ctx);
            let type_die = global_ctx.type_dies[&Type::Custom(typ)];
            let left_offset = type_size - primitive_size - offset as usize;
            // -> type, value, element-index
            expr.op_constu(primitive_size as u64 * 8);
            expr.op(gimli::DW_OP_mul); // -> type, value, (bit-)index
            // -> type, value, index
            // mask value
            expr.op(gimli::DW_OP_swap); // -> type, index, value
            expr.op_constu(primitive_mask(primitive));
            expr.op(gimli::DW_OP_and); // -> type, index, value
            // convert value to our type
            expr.op_convert(Some(type_die));
            expr.op(gimli::DW_OP_swap); // -> type, value, index
            // shift into correct position, subtracting the converted index
            expr.op_constu(left_offset as u64 * 8); // -> type, value, index, offset
            expr.op(gimli::DW_OP_swap); // -> type, value, offset, index
            expr.op(gimli::DW_OP_minus); // -> type, index, value, offsetindex
            expr.op_convert(Some(type_die));
            expr.op(gimli::DW_OP_dup); // -> type, value, offsetindex, offsetindex
            expr.op(gimli::DW_OP_rot); // -> type, offsetindex, value, offsetindex
            expr.op(gimli::DW_OP_shl); // -> type, offsetindex, offsetvalue
            // zero-out our "struct" by ANDing a mask
            expr.op(gimli::DW_OP_rot); // -> offsetvalue, type, offsetindex
            let mut mask = vec![0x00u8; type_size];
            mask[..primitive_size].fill(0xff);
            expr.op_const_type(type_die, mask.into_boxed_slice()); // -> offsetvalue, type, offsetindex, not_mask
            expr.op(gimli::DW_OP_not); // -> offsetvalue, type, offsetindex, mask
            expr.op(gimli::DW_OP_swap); // -> offsetvalue, type, mask, offsetindex
            expr.op(gimli::DW_OP_shl); // -> offsetvalue, type, offsetmask
            expr.op(gimli::DW_OP_and); // -> offsetvalue, masked_type
            // write field by ORing struct and value
            expr.op(gimli::DW_OP_or);
        }
        Instruction::GetIndex(path) => {
            assert!(path.path.last().unwrap().1.is_none(), "#getindex's last field must not have an index: {path}");
            let Path { typ, mut path } = path;
            path.last_mut().unwrap().1 = Some(0);
            let (offset, primitive) = field_offset(typ, &path, global_ctx);
            let primitive_size = primitive_size(primitive);
            let type_size = type_size(Type::Custom(typ), global_ctx);
            let left_offset = type_size as u8 - primitive_size as u8 - offset;
            let type_die = global_ctx.type_dies[&Type::Custom(typ)];
            // -> type, element-index
            expr.op_constu(primitive_size as u64 * 8);
            expr.op(gimli::DW_OP_mul); // -> type, (bit-)index
            expr.op_constu(left_offset as u64 * 8); // -> type, index, offset
            expr.op(gimli::DW_OP_swap); // -> type, offset, index
            expr.op(gimli::DW_OP_minus); // -> type, offsetindex
            expr.op_convert(Some(type_die));
            expr.op(gimli::DW_OP_shr);
            expr.op_convert(None);
            expr.op_constu(primitive_mask(primitive));
            expr.op(gimli::DW_OP_and);
        }
    }
}

fn int_to_type_array(val: u64, type_size: usize) -> Box<[u8]> {
    let mut vec = vec![0u8; type_size];
    let max_val = if type_size >= 8 {
        u64::MAX
    } else {
        1u64 << (type_size*8) - 1
    };
    if val > max_val {
        panic!("value {val} doesn't fit into type of size {type_size}");
    }
    let val = val.to_le_bytes();
    let len = val.len().min(type_size);
    vec[..len].copy_from_slice(&val[..len]);
    vec.into_boxed_slice()

}

fn init_primitive(primitive: Primitive, value: U64, vec: &mut VecDeque<u8>) {
    let size = primitive_size(primitive);
    let value = primitive_try_from_u64(primitive, value.number)
        .unwrap_or_else(|| panic!("{value} doesn't fit into {primitive}"));
    for byte in value.to_le_bytes()[..size].iter().copied().rev() {
        vec.push_front(byte);
    }
}
fn init_custom_type(init: CustomTypeInit, vec: &mut VecDeque<u8>, global_ctx: &GlobalContext<'_>) {
    let typ = &global_ctx.custom_types[&init.name];
    assert_eq!(
        init.fields.len(), typ.fields.len(),
        "#create {} has different number of fields: expected {}, found {}",
        init.name, typ.fields.len(), init.fields.len(),
    );
    for ((exname, extyp), (name, value)) in typ.fields.iter().copied().zip(init.fields) {
        assert_eq!(
            exname, name,
            "fields in #create {} have different names / order: expected {}, found {}",
            init.name, exname, name,
        );
        match (extyp, value) {
            (Type::Primitive(primitive), TypeInit::Primitive(value)) => {
                init_primitive(primitive, value, vec);
            }
            (Type::Array(primitive, len1), TypeInit::Array(value, len2)) => {
                assert_eq!(
                    len1, len2,
                    "invalid array initializer: expected length {len1}, found length {len2}",
                );
                for _ in 0..len1 {
                    init_primitive(primitive, value, vec);
                }
            }
            (Type::Custom(name), TypeInit::Custom(init)) => {
                assert_eq!(
                    name, init.name,
                    "invalid custom type in create: expected {name}, found {}", init.name,
                );
                init_custom_type(init, vec, global_ctx);
            }
            (ex @ Type::Primitive(_), found @ TypeInit::Array(_, _))
            | (ex @ Type::Primitive(_), found @ TypeInit::Custom(_))
            | (ex @ Type::Custom(_), found @ TypeInit::Primitive(_))
            | (ex @ Type::Custom(_), found @ TypeInit::Array(_, _))
            | (ex @ Type::Array(_, _), found @ TypeInit::Primitive(_))
            | (ex @ Type::Array(_, _), found @ TypeInit::Custom(_))
            => {
                panic!("invalid type in create: expected {ex}, found {found}");
            }
        }
    }
}

fn primitive_mask(primitive: Primitive) -> u64 {
    match primitive {
        Primitive::U8 | Primitive::I8 => u8::MAX as u64,
        Primitive::U16 | Primitive::I16 => u16::MAX as u64,
        Primitive::U32 | Primitive::I32 | Primitive::F32 => u32::MAX as u64,
        Primitive::U64 | Primitive::I64 | Primitive::F64 => u64::MAX,
    }
}
fn primitive_try_from_u64(primitive: Primitive, value: u64) -> Option<u64> {
    match primitive {
        Primitive::U8 => u8::try_from(value).map(|v| v as u64).ok(),
        Primitive::U16 => u16::try_from(value).map(|v| v as u64).ok(),
        Primitive::U32 => u32::try_from(value).map(|v| v as u64).ok(),
        Primitive::U64 => u64::try_from(value).map(|v| v as u64).ok(),
        Primitive::I8 => i8::try_from(value).map(|v| v as u64).ok(),
        Primitive::I16 => i16::try_from(value).map(|v| v as u64).ok(),
        Primitive::I32 => i32::try_from(value).map(|v| v as u64).ok(),
        Primitive::I64 => i64::try_from(value).map(|v| v as u64).ok(),
        Primitive::F32 => unimplemented!(),
        Primitive::F64 => unimplemented!(),
    }
}

fn type_size(typ: Type<'_>, global_ctx: &GlobalContext) -> usize {
    type_size_internal(typ, global_ctx, &mut Vec::new())
}
fn primitive_size(primitive: Primitive) -> usize {
    match primitive {
        Primitive::U8 => 1,
        Primitive::U16 => 2,
        Primitive::U32 => 4,
        Primitive::U64 => 8,
        Primitive::I8 => 1,
        Primitive::I16 => 2,
        Primitive::I32 => 4,
        Primitive::I64 => 8,
        Primitive::F32 => 4,
        Primitive::F64 => 8,
    }
}
fn type_size_internal<'a>(typ: Type<'a>, global_ctx: &GlobalContext<'a>, visited_types: &mut Vec<&'a str>) -> usize {
    match typ {
        Type::Primitive(primitive) => primitive_size(primitive),
        Type::Array(primitive, len) => primitive_size(primitive) * len,
        Type::Custom(name) => {
            if visited_types.contains(&name) {
                panic!("recursive type {name} found in {}", visited_types.join(" -> "));
            }
            visited_types.push(name);
            let size = global_ctx.custom_types[name].fields.iter().copied()
                .map(|(_name, typ)| type_size_internal(typ, global_ctx, visited_types))
                .sum();
            assert_eq!(visited_types.pop(), Some(name));
            size
        }
    }
}
fn field_offset(typ: &str, path: &[(&str, Option<usize>)], global_ctx: &GlobalContext<'_>) -> (u8, Primitive) {
    field_offset_internal(Type::Custom(typ), None, path, global_ctx)
}
fn field_offset_internal(typ: Type<'_>, index: Option<usize>, path: &[(&str, Option<usize>)], global_ctx: &GlobalContext<'_>) -> (u8, Primitive) {
    let type_name = match typ {
        Type::Custom(name) => name,
        Type::Array(primitive, len) => {
            assert!(path.is_empty(), "field access reached array `{typ}`, but field accesses are left: .{}", path.iter().map(|(field, _)| field).join("."));
            assert!(index.is_some(), "tried to access an array-field without index: {typ:?}");
            let index = index.unwrap();
            assert!(index < len, "index `{index}` out of bounds len `{len}`");
            let primitive_size = primitive_size(primitive);
            return ((primitive_size * index).try_into().unwrap(), primitive);
        }
        Type::Primitive(primitive) => {
            assert!(path.is_empty(), "field access reached primitive, but field accesses are left: .{}", path.iter().map(|(field, _)| field).join("."));
            return (0, primitive)
        }
    };
    let typ = global_ctx.custom_types.get(type_name)
        .unwrap_or_else(|| panic!("unknown type {type_name}"));
    assert!(!path.is_empty(), "field access path must end with a primitive, but instead ended with non-primitive {type_name}");
    assert!(index.is_none(), "field access path has an index for non-array {type_name}, index {}", index.unwrap());

    let mut offset = 0u8;
    let (field_name, index) = path[0];
    for &(name, typ) in &typ.fields {
        if name == field_name {
            let (o, primitive) = field_offset_internal(typ, index, &path[1..], global_ctx);
            return (offset.checked_add(o).unwrap(), primitive);
        }
        offset = offset.checked_add(type_size(typ, global_ctx).try_into().unwrap()).unwrap();
    }
    panic!("field {} not found in type {type_name}", path[0].0)
}
