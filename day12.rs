#!/usr/bin/env -S cargo +nightly -Zscript
fn main() {
    let file = std::fs::read_to_string("days/inputs/day12_input").unwrap();
    // skip pieces
    let mut lines = file.lines().skip(30);

    let mut count9 = 0;
    let mut countsub = 0;
    let mut count9square = 0;

    for line in lines {
        if line == "" {
            continue;
        }

        let width: i32 = line[..2].parse().unwrap();
        let height: i32 = line[3..5].parse().unwrap();
        let a: i32 = line[7..9].parse().unwrap();
        let b: i32 = line[10..12].parse().unwrap();
        let c: i32 = line[13..15].parse().unwrap();
        let d: i32 = line[16..18].parse().unwrap();
        let e: i32 = line[19..21].parse().unwrap();
        let f: i32 = line[22..24].parse().unwrap();
        println!("{:?}", (width, height, a, b, c, d, e, f));

        if (a+b+c+d+e+f)*9 <= width*height {
            count9 += 1;
        }
        if a*7 + b*6 + c*7 + d*7 + e*7 + f*5 <= width*height {
            countsub += 1;
        }
        if (a+b+c+d+e+f) <= (width/3) * (height/3) {
            count9square += 1;
        }
    }
    println!("{count9} {countsub} {count9square}");
}
