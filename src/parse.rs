use std::borrow::Cow;
use std::fmt::Write;
use std::fmt::{Binary, Display, Formatter, LowerHex, Octal};
use std::num::ParseIntError;
use std::ops::Neg;
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{extra, IterParser, Parser as _};
use chumsky::prelude::{any, choice, just, one_of, recursive, Rich};
use chumsky::text::newline;
use indent_write::fmt::IndentWriter;
use itertools::Itertools;

trait Parser<'a, T>: chumsky::Parser<'a, &'a str, T, extra::Err<Rich<'a, char>>> {}
impl<'a, T, P: chumsky::Parser<'a, &'a str, T, extra::Err<Rich<'a, char>>>> Parser<'a, T> for P {}

#[derive(Debug, Clone)]
pub enum Item<'a> {
    Rodata {
        name: &'a str,
        def_data: Vec<DefineData<'a>>,
    },
    Type(CustomType<'a>),
    Procedure {
        name: &'a str,
        body: Vec<Instruction<'a>>,
    },
    Variable {
        name: &'a str,
        body: Vec<Instruction<'a>>,
    },
}

#[derive(Debug, Clone)]
pub enum Instruction<'a> {
    /// DW_OP_addr
    Addr(Addr<'a>),
    /// `DW_OP_lit<n>`, `DW_OP_const<n>u`, `DW_OP_constu`
    Constu(U64),
    /// `DW_OP_const<n>s`, `DW_OP_consts`
    Consts(I64),
    // DW_OP_addrx
    // DW_OP_constx

    /// typ, data
    ConstType(Type<'a>, DefineData<'a>),

    // NOT POSSIBLE for AoC:
    // * DW_OP_fbreg
    // * DW_OP_breg<n>
    // * DW_OP_bregx
    // * DW_OP_regval_type

    /// DW_OP_dup
    Dup,
    /// DW_OP_drop
    Drop,
    /// DW_OP_pick, DW_OP_over
    Pick(U8),
    /// DW_OP_swap
    Swap,
    /// DW_OP_rot
    Rot,
    /// DW_OP_deref
    Deref,
    /// DW_OP_deref_size
    DerefSize(U8),
    // DW_OP_deref_type
    // DW_OP_xderef
    // DW_OP_xderef_size
    // DW_OP_xderef_type
    // NOT POSSIBLE for AoC:
    // * DW_OP_push_object_address
    // * DW_OP_form_tls_address
    // * DW_OP_call_frame_cfa
    /// DW_OP_abs
    Abs,
    /// DW_OP_and
    And,
    /// DW_OP_div
    Div,
    /// DW_OP_minus
    Minus,
    /// DW_OP_mod
    Mod,
    /// DW_OP_mul
    Mul,
    /// DW_OP_neg
    Neg,
    /// DW_OP_not
    Not,
    /// DW_OP_or
    Or,
    /// DW_OP_plus
    Plus,
    /// DW_OP_plus_uconst
    PlusUconst(U64),
    /// DW_OP_shl
    Shl,
    /// DW_OP_shr
    Shr,
    /// DW_OP_shra
    Shra,
    /// DW_OP_xor
    Xor,
    /// DW_OP_le
    Le,
    /// DW_OP_ge
    Ge,
    /// DW_OP_eq
    Eq,
    /// DW_OP_lt
    Lt,
    /// DW_OP_gt
    Gt,
    /// DW_OP_ne
    Ne,
    /// DW_OP_skip
    Skip(&'a str),
    /// DW_OP_bra
    Bra(&'a str),
    /// DW_OP_call
    Call(&'a str),
    // NOT RELEVANT for AoC: DW_OP_call_ref
    /// DW_OP_convert
    Convert(TypeOrGeneric<'a>),
    /// DW_OP_reinterpret
    Reinterpret(TypeOrGeneric<'a>),
    /// DW_OP_nop
    Nop,
    // DW_OP_entry_value
    // NOT POSSIBLE for AoC:
    // * DW_OP_reg<n>
    // * DW_OP_regx
    // DW_OP_implicit_value
    // DW_OP_stack_value
    // DW_OP_implicit_pointer
    // DW_OP_piece
    // DW_OP_bit_piece

    // CUSTOM "INSTRUCTIONS"
    Label(&'a str),
    /// prints the current value-stack
    ///
    /// encoded using DW_OP_addr __debug_stack; DW_OP_drop
    Debug,
    /// Condition-Lhs, Condition-Op, Condition-Rhs, Then, Else
    IfElse(Vec<(Vec<Instruction<'a>>, CondOp, Vec<Instruction<'a>>, Vec<Instruction<'a>>)>, Vec<Instruction<'a>>),
    /// Create and push a new instance of a custom type
    Create(CustomTypeInit<'a>),
    /// Pop a type from the stack and push the value of a (possibly nested) field
    Get(Path<'a>),
    /// Pop a value from the stack and set it as (possibly nested) field of the now-topmost type
    Set(Path<'a>),
    /// Pop an index and a type from the stack and push the value of the (possibly nested) array-element at the given index
    GetIndex(Path<'a>),
    /// Pop an index and a value from the stack and set the value as the (possibly nested) array-element at the given index of the now-topmost type
    SetIndex(Path<'a>),
}

#[derive(Debug, Clone, Copy)]
pub enum Addr<'a> {
    RodataRef(&'a str, i64),
    Addr(U64),
}

#[derive(Debug, Clone, Copy)]
pub enum CondOp {
    Lt,
    Le,
    Eq,
    Ge,
    Gt,
    Ne,
}

/// A possibly nested field of a type
///
/// `$Foo.bar[0].baz` becomes `Path { typ: "$Foo", path: vec![("bar", Some(0), ("baz", None)] }`.
#[derive(Debug, Clone)]
pub struct Path<'a> {
    pub typ: &'a str,
    pub path: Vec<(&'a str, Option<usize>)>,
}

impl Display for Item<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Rodata { name, def_data: data } => {
                writeln!(f, "#rodata {name} {{")?;
                let mut indented = IndentWriter::new("    ", &mut *f);
                for data in data {
                    writeln!(indented, "{data}")?;
                }
                writeln!(f, "}}")
            }
            Item::Type(custom) => write!(f, "#typ {custom}"),
            Item::Procedure { name, body } => {
                writeln!(f, "#proc {name} {{")?;
                let mut indented = IndentWriter::new("    ", &mut *f);
                for instruction in body {
                    writeln!(indented, "{instruction}")?;
                }
                writeln!(f, "}}")
            },
            Item::Variable { name, body } => {
                writeln!(f, "#var {name} {{")?;
                let mut indented = IndentWriter::new("    ", &mut *f);
                for instruction in body {
                    writeln!(indented, "{instruction}")?;
                }
                writeln!(f, "}}")
            },
        }
    }
}

impl Display for Instruction<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Addr(addr) => Display::fmt(addr, f),
            Instruction::Constu(unsigned) => write!(f, "constu {unsigned}"),
            Instruction::Consts(signed) => write!(f, "consts {signed}"),
            Instruction::ConstType(typ, data) => write!(f, "const_type {typ}, {data}"),
            Instruction::Dup => write!(f, "dup"),
            Instruction::Drop => write!(f, "drop"),
            Instruction::Pick(index) => write!(f, "pick {index}"),
            Instruction::Swap => write!(f, "swap"),
            Instruction::Rot => write!(f, "rot"),
            Instruction::Deref => write!(f, "deref"),
            Instruction::DerefSize(size) => write!(f, "deref_size {size}"),
            Instruction::Abs => write!(f, "abs"),
            Instruction::And => write!(f, "and"),
            Instruction::Div => write!(f, "div"),
            Instruction::Minus => write!(f, "minus"),
            Instruction::Mod => write!(f, "mod"),
            Instruction::Mul => write!(f, "mul"),
            Instruction::Neg => write!(f, "neg"),
            Instruction::Not => write!(f, "not"),
            Instruction::Or => write!(f, "or"),
            Instruction::Plus => write!(f, "plus"),
            Instruction::PlusUconst(unsigned) => write!(f, "plus_uconst {unsigned}"),
            Instruction::Shl => write!(f, "shl"),
            Instruction::Shr => write!(f, "shr"),
            Instruction::Shra => write!(f, "shra"),
            Instruction::Xor => write!(f, "xor"),
            Instruction::Le => write!(f, "le"),
            Instruction::Ge => write!(f, "ge"),
            Instruction::Eq => write!(f, "eq"),
            Instruction::Lt => write!(f, "lt"),
            Instruction::Gt => write!(f, "gt"),
            Instruction::Ne => write!(f, "ne"),
            Instruction::Skip(label) => write!(f, "skip {label}"),
            Instruction::Bra(label) => write!(f, "bra {label}"),
            Instruction::Call(name) => write!(f, "call {name}"),
            Instruction::Convert(typ) => write!(f, "convert {typ}"),
            Instruction::Reinterpret(typ) => write!(f, "reinterpret {typ}"),
            Instruction::Nop => write!(f, "nop"),
            Instruction::Label(label) => write!(f, "{label}:"),
            Instruction::Debug => write!(f, "#debug"),
            Instruction::IfElse(ifs, els) => {
                for (i, (lhs, op, rhs, then)) in ifs.iter().enumerate() {
                    write!(f, "#if (")?;
                    for lhs in lhs {
                        write!(f, "{lhs}, ")?;
                    }
                    write!(f, ") {op} (")?;
                    for rhs in rhs {
                        write!(f, "{rhs}, ")?;
                    }
                    writeln!(f, ") {{")?;
                    let mut indented = IndentWriter::new("    ", &mut *f);
                    for then in then {
                        writeln!(indented, "{then}")?;
                    }
                    write!(f, "}}")?;
                    if i < ifs.len() - 1 {
                        write!(f, " #else ")?;
                    } else {
                        writeln!(f)?;
                    }
                }
                if !els.is_empty() {
                    writeln!(f, " else {{")?;
                    let mut indented = IndentWriter::new("    ", &mut *f);
                    for els in els {
                        writeln!(indented, "{els}")?;
                    }
                    writeln!(f, "}}")?;
                }
                Ok(())
            },
            Instruction::Create(custom_type_init) => Display::fmt(custom_type_init, f),
            Instruction::Get(path) => write!(f, "#get {path}"),
            Instruction::Set(path) => write!(f, "#set {path}"),
            Instruction::GetIndex(path) => write!(f, "#getindex {path}"),
            Instruction::SetIndex(path) => write!(f, "#setindex {path}"),
        }
    }
}

impl Display for Addr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            Addr::RodataRef(name, addend) => write!(f, "{name}+{addend}"),
            Addr::Addr(addr) => Display::fmt(&addr, f),
        }
    }
}

impl Display for CondOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CondOp::Lt => write!(f, "<"),
            CondOp::Le => write!(f, "<="),
            CondOp::Eq => write!(f, "=="),
            CondOp::Ge => write!(f, ">="),
            CondOp::Gt => write!(f, ">"),
            CondOp::Ne => write!(f, "!="),
        }
    }
}
impl Display for Path<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Path { typ, path } = self;
        write!(f, "{typ}")?;
        for (field, index) in path {
            write!(f, ".{field}")?;
            if let Some(index) = index {
                write!(f, "[{index}]")?;
            }
        }
        Ok(())
    }
}

pub fn parse(src: &str) -> Vec<Item<'_>> {
    match items().parse(src).into_result() {
        Ok(ast) => ast,
        Err(parse_errs) => {
            for e in parse_errs {
                Report::build(ReportKind::Error, ((), e.span().into_range()))
                    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                    .with_message(e.to_string())
                    .with_label(
                        Label::new(((), e.span().into_range()))
                            .with_message(e.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .print(Source::from(&src))
                    .unwrap()
            }
            panic!("encountered errors");
        },
    }
}

fn items<'a>() -> impl Parser<'a, Vec<Item<'a>>> {
    choice((
        just("#rodata")
            .ignore_then(label().padded())
            .then(define_data().padded().padded_by(comment().repeated()).repeated().collect().delimited_by(just('{'), just('}')))
            .map(|(name, def_data)| Item::Rodata { name, def_data }),
        just("#type").padded()
            .ignore_then(custom_type())
            .map(Item::Type),
        just("#proc")
            .ignore_then(label().padded())
            .then(instructions(instruction()).padded().delimited_by(just('{'), just("}")))
            .map(|(name, body)| Item::Procedure { name, body }),
        just("#var")
            .ignore_then(label().padded())
            .then(instructions(instruction()).padded().delimited_by(just('{'), just("}")))
            .map(|(name, body)| Item::Variable { name, body }),
    )).padded_by(comment().repeated()).padded().repeated().collect()
}

fn instructions<'a>(instruction: impl Parser<'a, Instruction<'a>> + Clone) -> impl Parser<'a, Vec<Instruction<'a>>> + Clone {
    instruction.padded().padded_by(comment().repeated()).padded().repeated().collect()
}

fn comment<'a>() -> impl Parser<'a, ()> + Clone {
    just("//").then(any().and_is(newline().not()).repeated()).then(newline()).ignored()
}

fn whitespace<'a>() -> impl Parser<'a, ()> + Clone {
    chumsky::text::inline_whitespace().at_least(1)
}

fn instruction<'a>() -> impl Parser<'a, Instruction<'a>> + Clone {
    recursive(|instruction| choice((
        just("addr").ignore_then(whitespace())
            .ignore_then(choice((
                u64().map(Addr::Addr),
                label().then(choice((
                    just('+').padded().ignore_then(u64()).map(|u| u.number as i64),
                    just('-').padded().ignore_then(u64()).map(|u| -(u.number as i64)),
                )).or_not()).map(|(name, addend)| Addr::RodataRef(name, addend.unwrap_or(0))),
            ))).map(Instruction::Addr),
        just("constu").ignore_then(whitespace()).ignore_then(u64()).map(Instruction::Constu),
        just("consts").ignore_then(whitespace()).ignore_then(i64()).map(Instruction::Consts),
        just("const_type").ignore_then(whitespace())
            .ignore_then(typ().padded())
            .then_ignore(just(','))
            .then(define_data().padded())
            .map(|(typ, data)| Instruction::ConstType(typ, data)),
        just("dup").to(Instruction::Dup),
        just("drop").to(Instruction::Drop),
        just("pick").ignore_then(whitespace()).ignore_then(u8()).map(Instruction::Pick),
        just("swap").to(Instruction::Swap),
        just("rot").to(Instruction::Rot),
        just("deref_size").ignore_then(whitespace()).ignore_then(u8()).map(Instruction::DerefSize),
        just("deref").to(Instruction::Deref),
        just("abs").to(Instruction::Abs),
        just("and").to(Instruction::And),
        just("div").to(Instruction::Div),
        just("minus").to(Instruction::Minus),
        just("mod").to(Instruction::Mod),
        just("mul").to(Instruction::Mul),
        just("neg").to(Instruction::Neg),
        just("not").to(Instruction::Not),
        just("or").to(Instruction::Or),
        just("plus").to(Instruction::Plus),
        just("plus_uconst").ignore_then(whitespace()).ignore_then(u64()).map(Instruction::PlusUconst),
        just("shl").to(Instruction::Shl),
        just("shra").to(Instruction::Shra),
        just("shr").to(Instruction::Shr),
        just("xor").to(Instruction::Xor),
   )).or(choice((
        just("le").to(Instruction::Le),
        just("ge").to(Instruction::Ge),
        just("eq").to(Instruction::Eq),
        just("lt").to(Instruction::Lt),
        just("gt").to(Instruction::Gt),
        just("ne").to(Instruction::Ne),
        just("skip").ignore_then(whitespace()).ignore_then(label()).map(Instruction::Skip),
        just("bra").ignore_then(whitespace()).ignore_then(label()).map(Instruction::Bra),
        just("call").ignore_then(whitespace()).ignore_then(label()).map(Instruction::Call),
        just("convert").ignore_then(whitespace()).ignore_then(typ_or_generic()).map(Instruction::Convert),
        just("reinterpret").ignore_then(whitespace()).ignore_then(typ_or_generic()).map(Instruction::Reinterpret),
        just("nop").to(Instruction::Nop),
        label().then_ignore(just(':')).map(Instruction::Label),
        just("#debug").to(Instruction::Debug),
        just("#if")
            .padded().padded_by(comment().repeated())
            .ignore_then(instruction.clone().padded().separated_by(just(',')).allow_trailing().collect::<Vec<_>>().delimited_by(just('('), just(')')))
            .padded().padded_by(comment().repeated())
            .then(cond_op().padded())
            .padded().padded_by(comment().repeated())
            .then(instruction.clone().padded().separated_by(just(',')).allow_trailing().collect::<Vec<_>>().delimited_by(just('('), just(')')))
            .padded().padded_by(comment().repeated())
            .then_ignore(just('{').padded())
            .padded().padded_by(comment().repeated())
            .then(instructions(instruction.clone()))
            .padded().padded_by(comment().repeated())
            .map(|(((lhs, cond_op), rhs), then)| (lhs, cond_op, rhs, then))
            .padded().padded_by(comment().repeated())
            .then_ignore(just('}').padded())
            .padded().padded_by(comment().repeated())
            .separated_by(just("#else"))
            .at_least(1)
            .collect::<Vec<_>>()
            .then(
                just("#else")
                    .padded()
                    .ignore_then(just('{').padded())
                    .ignore_then(instructions(instruction.clone()))
                    .then_ignore(just('}').padded())
                    .or_not()
                    .map(Option::unwrap_or_default)
            ).map(|(ifs, els)| Instruction::IfElse(ifs, els)),
        just("#create").padded()
            .ignore_then(custom_type_init())
            .map(Instruction::Create),
        just("#getindex").padded()
            .ignore_then(path())
            .map(Instruction::GetIndex),
        just("#setindex").padded()
            .ignore_then(path())
            .map(Instruction::SetIndex),
        just("#get").padded()
            .ignore_then(path())
            .map(Instruction::Get),
        just("#set").padded()
            .ignore_then(path())
            .map(Instruction::Set),
    ))))
}

fn path<'a>() -> impl Parser<'a, Path<'a>> + Clone {
    just('$').then(ident()).to_slice()
        .then_ignore(just('.'))
        .then(
            ident().then(
                just('[').padded().ignore_then(u64().padded()).then_ignore(just(']').padded()).or_not()
                    .map(|num| num.map(|num| usize::try_from(num.number).unwrap()))
            ).separated_by(just('.')).at_least(1).collect()
        ).map(|(typ, path)| Path { typ, path })
}

fn cond_op<'a>() -> impl Parser<'a, CondOp> + Clone {
    choice((
        just('<').to(CondOp::Lt),
        just("<=").to(CondOp::Le),
        just("==").to(CondOp::Eq),
        just(">=").to(CondOp::Ge),
        just('>').to(CondOp::Gt),
        just("!=").to(CondOp::Ne),
    ))
}

fn ident<'a>() -> impl Parser<'a, &'a str> + Clone {
    one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_").repeated().at_least(1).to_slice()
}
fn label<'a>() -> impl Parser<'a, &'a str> + Clone {
    one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._").repeated().at_least(1).to_slice()
}

#[derive(Debug, Clone)]
pub enum DefineData<'a> {
    U8(Vec<U8>),
    U16(Vec<U16>),
    U32(Vec<U32>),
    U64(Vec<U64>),
    I8(Vec<I8>),
    I16(Vec<I16>),
    I32(Vec<I32>),
    I64(Vec<I64>),
    IncludeFile(Cow<'a, str>),
}
impl Display for DefineData<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DefineData::U8(u) => write!(f, "#u8 {}", u.iter().join(", ")),
            DefineData::U16(u) => write!(f, "#u16 {}", u.iter().join(", ")),
            DefineData::U32(u) => write!(f, "#u32 {}", u.iter().join(", ")),
            DefineData::U64(u) => write!(f, "#u64 {}", u.iter().join(", ")),
            DefineData::I8(i) => write!(f, "#i8 {}", i.iter().join(", ")),
            DefineData::I16(i) => write!(f, "#i16 {}", i.iter().join(", ")),
            DefineData::I32(i) => write!(f, "#i32 {}", i.iter().join(", ")),
            DefineData::I64(i) => write!(f, "#i64 {}", i.iter().join(", ")),
            DefineData::IncludeFile(path) => write!(f, "#include_file \"{path:?}\""),
        }
    }
}
impl DefineData<'_> {
    pub fn to_vec(&self) -> Vec<u8> {
        match self {
            DefineData::U8(u) => u.into_iter().flat_map(|u| u.number.to_le_bytes()).collect(),
            DefineData::U16(u) => u.into_iter().flat_map(|u| u.number.to_le_bytes()).collect(),
            DefineData::U32(u) => u.into_iter().flat_map(|u| u.number.to_le_bytes()).collect(),
            DefineData::U64(u) => u.into_iter().flat_map(|u| u.number.to_le_bytes()).collect(),
            DefineData::I8(i) => i.into_iter().flat_map(|u| u.number.to_le_bytes()).collect(),
            DefineData::I16(i) => i.into_iter().flat_map(|u| u.number.to_le_bytes()).collect(),
            DefineData::I32(i) => i.into_iter().flat_map(|u| u.number.to_le_bytes()).collect(),
            DefineData::I64(i) => i.into_iter().flat_map(|u| u.number.to_le_bytes()).collect(),
            DefineData::IncludeFile(path) => std::fs::read(&**path).unwrap_or_else(|e| panic!("error loading rodata from `{path}`: {e}")),
        }
    }
}

fn define_data<'a>() -> impl Parser<'a, DefineData<'a>> + Clone {
    choice((
        just("#u8").padded().ignore_then(u8().padded().separated_by(just(',')).collect()).map(DefineData::U8),
        just("#u16").padded().ignore_then(u16().padded().separated_by(just(',')).collect()).map(DefineData::U16),
        just("#u32").padded().ignore_then(u32().padded().separated_by(just(',')).collect()).map(DefineData::U32),
        just("#u64").padded().ignore_then(u64().padded().separated_by(just(',')).collect()).map(DefineData::U64),
        just("#i8").padded().ignore_then(i8().padded().separated_by(just(',')).collect()).map(DefineData::I8),
        just("#i16").padded().ignore_then(i16().padded().separated_by(just(',')).collect()).map(DefineData::I16),
        just("#i32").padded().ignore_then(i32().padded().separated_by(just(',')).collect()).map(DefineData::I32),
        just("#i64").padded().ignore_then(i64().padded().separated_by(just(',')).collect()).map(DefineData::I64),
        just("#include_file").padded().ignore_then(dqstring()).map(DefineData::IncludeFile),
    ))
}

fn dqstring<'a>() -> impl Parser<'a, Cow<'a, str>> + Clone {
    just('\\').then(any())
        .ignored()
        .or(any().and_is(just('"').not()).ignored())
        .repeated()
        .to_slice()
        .delimited_by(just('"'), just('"'))
        .map(unescape)
}

fn unescape(s: &str) -> Cow<'_, str> {
    let mut string = None;
    let mut char_indices = s.char_indices();
    while let Some((i, chr)) = char_indices.next() {
        if chr == '\\' {
            string.get_or_insert_with(|| String::from(&s[..i]))
                .push(char_indices.next().unwrap().1);
        } else if let Some(string) = &mut string {
            string.push(chr);
        }
    }

    string.map(Cow::Owned).unwrap_or(Cow::Borrowed(s))
}

#[derive(Debug, Clone, Copy)]
pub struct Number<T> {
    pub radix: Radix,
    pub number: T,
}
impl<T: Display + Binary + Octal + LowerHex> Display for Number<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.radix {
            Radix::Dec => write!(f, "{}", self.number),
            Radix::Bin => write!(f, "{:#b}", self.number),
            Radix::Oct => write!(f, "{:#o}", self.number),
            Radix::Hex => write!(f, "{:#x}", self.number),
        }
    }
}
#[derive(Debug, Clone, Copy)]
pub enum Radix {
    Dec = 10,
    Bin = 2,
    Oct = 8,
    Hex = 16,
}
impl Radix {
    fn prefix(self) -> &'static str {
        match self {
            Radix::Dec => "",
            Radix::Bin => "0b",
            Radix::Oct => "0o",
            Radix::Hex => "0x",
        }
    }
}

fn signed<'a, T: FromStrRadix + Neg<Output = T>>() -> impl Parser<'a, (T, Radix)> + Clone {
    just('-').or_not().then(unsigned::<T>())
        .map(|(neg, (number, radix))| (
            if neg.is_some() { -number } else { number },
            radix,
        ))
}
fn unsigned<'a, T: FromStrRadix>() -> impl Parser<'a, (T, Radix)> + Clone {
    choice((
       number_radix(Radix::Hex),
       number_radix(Radix::Oct),
       number_radix(Radix::Bin),
       number_radix(Radix::Dec),
    ))
}
fn number_radix<'a, T: FromStrRadix>(radix: Radix) -> impl Parser<'a, (T, Radix)> + Clone {
    let underscore = any().filter(|&c: &char| c == '_');
    let digit = any().filter(move |&c: &char| c.is_digit(radix as u32));
    choice((
        just(radix.prefix()).ignore_then(digit.padded_by(underscore.repeated()).repeated().at_least(1).to_slice()
            .map(move |s: &str| (T::from_str_radix(&s.replace('_', ""), radix as u32).unwrap(), radix))),
        just('\\')
            .ignore_then(one_of("'\"\\nrt").map(|c| match c {
                '\'' => '\'',
                '"' => '"',
                '\\' => '\\',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                _ => unreachable!("{c}"),
            })).delimited_by(just('\''), just('\''))
            .map(move |c| (T::from_char(c), radix)),
        any().delimited_by(just('\''), just('\''))
            .map(move |c| (T::from_char(c), radix)),
    ))
}

trait FromStrRadix: Sized {
    fn from_str_radix(s: &str, radix: u32) -> Result<Self, ParseIntError>;
    fn from_char(chr: char) -> Self;
}
macro_rules! impl_number {
    ($($f:ident $typ:tt => $inner:tt,)*) => {
        $(
            pub type $typ = Number<$inner>;
            impl FromStrRadix for $inner {
                fn from_str_radix(s: &str, radix: u32) -> Result<Self, ParseIntError>{
                    <$inner>::from_str_radix(s, radix)
                }
                fn from_char(chr: char) -> Self {
                    let val = u32::from(chr);
                    <$inner>::try_from(val).unwrap()
                }
            }
            fn $inner<'a>() -> impl Parser<'a, $typ> + Clone {
                $f().map(|(number, radix)| $typ { number, radix })
            }
        )*
    };
}
impl_number! {
    unsigned U8 => u8,
    unsigned U16 => u16,
    unsigned U32 => u32,
    unsigned U64 => u64,
    signed I8 => i8,
    signed I16 => i16,
    signed I32 => i32,
    signed I64 => i64,
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum TypeOrGeneric<'a> {
    Generic,
    Type(Type<'a>),
}
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum Type<'a> {
    Primitive(Primitive),
    /// Element-Type, number-of-elements
    Array(Primitive, usize),
    Custom(&'a str),
}
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum Primitive {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
}
#[derive(Debug, Clone)]
pub struct CustomType<'a> {
    pub name: &'a str,
    pub fields: Vec<(&'a str, Type<'a>)>,
}
impl Display for TypeOrGeneric<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeOrGeneric::Generic => write!(f, "$generic"),
            TypeOrGeneric::Type(typ) => Display::fmt(typ, f),
        }
    }
}
impl Display for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Primitive(primitive) => Display::fmt(primitive, f),
            Type::Array(primitive, len) => write!(f, "[{primitive}; {len}]"),
            Type::Custom(custom) => Display::fmt(custom, f),
        }
    }
}
impl Display for Primitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::U8 => write!(f, "$u8"),
            Primitive::U16 => write!(f, "$u16"),
            Primitive::U32 => write!(f, "$u32"),
            Primitive::U64 => write!(f, "$u64"),
            Primitive::I8 => write!(f, "$i8"),
            Primitive::I16 => write!(f, "$i16"),
            Primitive::I32 => write!(f, "$i32"),
            Primitive::I64 => write!(f, "$i64"),
            Primitive::F32 => write!(f, "$f32"),
            Primitive::F64 => write!(f, "$f64"),
        }
    }
}
impl Display for CustomType<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {{", self.name)?;
        let mut indented = IndentWriter::new("    ", &mut *f);
        for (name, typ) in &self.fields {
            writeln!(indented, "{}: {},", name, typ)?;
        }
        writeln!(f, "}}")
    }
}

fn primitive_type<'a>() -> impl Parser<'a, Primitive> + Clone {
    choice((
        just("$u8").to(Primitive::U8),
        just("$u16").to(Primitive::U16),
        just("$u32").to(Primitive::U32),
        just("$u64").to(Primitive::U64),
        just("$i8").to(Primitive::I8),
        just("$i16").to(Primitive::I16),
        just("$i32").to(Primitive::I32),
        just("$i64").to(Primitive::I64),
        just("$f32").to(Primitive::F32),
        just("$f64").to(Primitive::F64),
    ))
}
fn array_type<'a>() -> impl Parser<'a, (Primitive, usize)> + Clone {
    just('[').padded()
        .ignore_then(primitive_type().padded())
        .then_ignore(just(';').padded())
        .then(u64().padded())
        .then_ignore(just(']').padded())
        .map(|(primitive, len)| (primitive, len.number.try_into().unwrap()))
}
fn custom_type<'a>() -> impl Parser<'a, CustomType<'a>> + Clone {
    just('$').ignore_then(ident()).to_slice()
        .padded()
        .then(
            field_def().separated_by(just(',').padded().padded_by(comment().repeated())).allow_trailing().collect()
                .delimited_by(just('{'), just('}'))
        ).map(|(name, fields)| CustomType { name, fields })
}
fn field_def<'a>() -> impl Parser<'a, (&'a str, Type<'a>)> + Clone {
    ident().padded()
        .then_ignore(just(':').padded())
        .then(typ().padded())
}
fn typ_or_generic<'a>() -> impl Parser<'a, TypeOrGeneric<'a>> + Clone {
    choice((
        just("$generic").to(TypeOrGeneric::Generic),
        typ().map(TypeOrGeneric::Type),
    ))
}
fn typ<'a>() -> impl Parser<'a, Type<'a>> + Clone {
    choice((
        primitive_type().map(Type::Primitive),
        array_type().map(|(primitive, len)| Type::Array(primitive, len)),
        just('$').ignore_then(ident()).to_slice().map(Type::Custom),
    ))
}


#[derive(Debug, Clone)]
pub enum TypeInit<'a> {
    Primitive(U64),
    /// element, len
    Array(U64, usize),
    Custom(CustomTypeInit<'a>),
}
#[derive(Debug, Clone)]
pub struct CustomTypeInit<'a> {
    pub name: &'a str,
    pub fields: Vec<(&'a str, TypeInit<'a>)>,
}
impl Display for TypeInit<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInit::Primitive(primitive) => Display::fmt(primitive, f),
            TypeInit::Array(value, len) => write!(f, "[{value}; {len}]"),
            TypeInit::Custom(custom) => Display::fmt(custom, f),
        }
    }
}
impl Display for CustomTypeInit<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {{", self.name)?;
        let mut indented = IndentWriter::new("    ", &mut *f);
        for (name, typ) in &self.fields {
            writeln!(indented, "{name}: {typ},")?;
        }
        writeln!(f, "}}")
    }
}

fn custom_type_init<'a>() -> impl Parser<'a, CustomTypeInit<'a>> + Clone {
    recursive(|custom_type_init| {
        let array_init = just('[').padded()
            .ignore_then(u64().padded())
            .then_ignore(just(';').padded())
            .then(u64().padded())
            .then_ignore(just(']').padded())
            .map(|(element, len)| TypeInit::Array(element, len.number.try_into().unwrap()));
        let typ_init = choice((
            u64().map(TypeInit::Primitive),
            array_init,
            custom_type_init.map(TypeInit::Custom),
        ));
        let field_init = ident().padded()
            .then_ignore(just(':').padded())
            .then(typ_init.padded());

        just('$').ignore_then(ident()).to_slice().padded()
            .then(
                field_init.separated_by(just(',').padded()).allow_trailing().collect()
                    .delimited_by(just('{'), just('}'))
            )
            .map(|(name, fields)| CustomTypeInit { name, fields })
    })
}
