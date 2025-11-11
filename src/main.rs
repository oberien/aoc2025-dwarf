use std::env;
use crate::compile::compile;
use crate::parse::parse;
use crate::write::DwarfProgram;

mod parse;
mod compile;
mod write;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let src = std::fs::read_to_string(env::args().nth(1).unwrap()).unwrap();
    let instructions = parse(&src);
    // for inst in &instructions {
    //     println!("{inst}");
    // }
    let mut program = DwarfProgram::new();
    compile(&mut program, instructions);
    program.write_to_file("aoc2025-dwarf.o")
}