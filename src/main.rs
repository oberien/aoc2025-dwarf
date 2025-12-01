use std::env;
use std::ffi::OsStr;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use clap::Parser;
use ignore::Walk;
use crate::compile::compile;
use crate::parse::parse;
use crate::dwarf_program::DwarfProgram;

#[cfg(test)]
mod tests;
mod parse;
mod compile;
mod dwarf_program;

#[derive(Parser, Debug)]
struct Args {
    file: Option<PathBuf>,
    #[clap(short, long = "out", long = "output")]
    output_file: Option<PathBuf>,
    #[clap(short)]
    debug: bool,
    #[clap(subcommand)]
    command: Option<Command>,
}

#[derive(clap::Subcommand, Debug, Clone, Default)]
enum Command {
    #[default]
    Compile,
    Run {
        #[clap(short = 'x')]
        hex: bool,
        die: String,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let Args { file, output_file, debug, command } = Args::parse();
    let mut src = String::new();
    match file {
        Some(path) => {
            File::open(path).expect("input file not found")
                .read_to_string(&mut src).expect("unable to read input file");
        },
        None => for file in Walk::new(".") {
            let file = file.unwrap();
            if file.file_type().is_none() || !file.file_type().unwrap().is_file() {
                continue;
            }
            if file.path().extension() != Some(OsStr::new("dwasm")) {
                continue;
            }
            println!("loading file {}", file.path().display());
            File::open(file.path()).unwrap_or_else(|e| panic!("can't open file {}: {e}", file.path().display()))
                .read_to_string(&mut src).unwrap_or_else(|e| panic!("can't read file {}: {e}", file.path().display()));
        }
    }
    let instructions = parse(&src);
    if debug {
        for inst in &instructions {
            println!("{inst}");
        }
    }
    let mut program = DwarfProgram::new();
    compile(&mut program, instructions);

    match command.unwrap_or_default() {
        Command::Compile => {
            let output = output_file.unwrap_or_else(|| PathBuf::from(format!("{}.o", env!("CARGO_PKG_NAME"))));
            program.write_to(File::create(output)?)?;
        }
        Command::Run { die, hex } => {
            let res = program.run(die);
            match hex {
                true => println!("{res:#x}"),
                false => println!("{res}"),
            }
        }
    }

    Ok(())
}