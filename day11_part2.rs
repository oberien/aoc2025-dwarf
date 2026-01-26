#!/usr/bin/env -S cargo +nightly -Zscript
---cargo
[dependencies]
indexmap = "2.13"
---

use std::collections::HashMap;
use indexmap::IndexSet;

struct Data {
    neighbors: HashMap<String, Vec<String>>,
    // depth -> node-name that must be contained in the path
    must_contain: HashMap<u8, String>,
    depths: HashMap<String, u8>,
    paths_cumulative: HashMap<String, u64>,
}

fn main() {
    let file = std::fs::read_to_string("days/inputs/day11_input").unwrap();
    let neighbors = file.lines().map(|line| {
        (line[..3].to_owned(), line[5..].split(" ").map(|s| s.to_owned()).collect())
    }).collect();

    let mut data = Data {
        neighbors,
        must_contain: [
            (9, "fft".to_owned()),
            (27, "dac".to_owned()),
        ].into_iter().collect(),
        depths: HashMap::new(),
        paths_cumulative: HashMap::new(),
    };
    data.neighbors.insert("out".to_owned(), Vec::new());
    data.paths_cumulative.insert("out".to_owned(), 1);
    calc_depth(&mut data);
    calc_paths(&mut data);
}

fn calc_paths(data: &mut Data) {
    let mut worklist: Vec<_> = data.depths.iter()
        .filter(|(node, _depth)| *node != "out")
        .map(|(node, depth)| (*depth, node))
        .collect();
    worklist.sort_unstable_by_key(|(depth, _node)| -(*depth as i8));

    println!("37 out: 1");
    for (depth, node) in worklist {
        let mut sum = 0;
        for neighbor in &data.neighbors[node] {
            if let Some(required) = data.must_contain.get(&(depth + 1)) && neighbor != required {
                continue;
            }
            sum += data.paths_cumulative[neighbor];
        }
        println!("{depth:02} {node}: {sum}");
        assert!(data.paths_cumulative.insert(node.clone(), sum).is_none());
    }
}

fn calc_depth(data: &mut Data) {
    let mut worklist: IndexSet<_> = data.neighbors.keys().collect();
    while let Some(node) = worklist.pop() {
        let depth = *data.depths.entry(node.to_owned()).or_default();
        for neighbor in &data.neighbors[node] {
            let current_depth = data.depths.entry(neighbor.clone()).or_default();
            let new_depth = (*current_depth).max(depth + 1);
            if new_depth != *current_depth {
                *current_depth = new_depth;
                worklist.insert(neighbor);
            }
        }
    }
}
