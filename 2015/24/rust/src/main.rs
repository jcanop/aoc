use itertools::Itertools;
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";

// --- Find the smalles Quantum Entanglement ---
fn quantum_entanglement(list: &Vec<usize>, groups: usize) -> usize {
    let target = list.iter().sum::<usize>() / groups;
    let mut min = std::usize::MAX;

    for i in 1 .. list.len() {
        for v in list.iter().combinations(i) {
            if v.iter().map(|x| **x).sum::<usize>() == target {
                min = std::cmp::min(min, v.iter().map(|x| **x).product())
            }
        }
        if min != std::usize::MAX { return min; }
    }
    panic!("Not found!");
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    let list: Vec<_> = BufReader::new(file).lines()
        .map(|line| line.unwrap().parse::<usize>().unwrap())
        .collect();

    // --- Puzzle 1 ---
    let qe = quantum_entanglement(&list, 3);
    println!("1. Quantum entaglement: {}", qe);

    // --- Puzzle 2 ---
    let qe = quantum_entanglement(&list, 4);
    println!("1. Quantum entaglement: {}", qe);

    Ok(())
}

