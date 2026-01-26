#!/usr/bin/env -S cargo +nightly -Zscript
fn main() {
    let file = std::fs::read_to_string("days/inputs/day11_input").unwrap();
    println!("digraph {{");
    for line in file.lines() {
        let src = &line[..3];
        assert_eq!(&line[3..5], ": ");
        for dst in line[5..].split(" ") {
            println!("{src} -> {dst};");
        }
    }
    println!("}}");
}
