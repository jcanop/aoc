use permutator::LargeCombinationIterator;
use std::collections::HashMap;
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const LITERS: usize = 150;

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut list: Vec<usize> = Vec::new();
    let mut count = 0;
    let mut map: HashMap<usize, usize> = HashMap::new();

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        list.push(line.parse()?);
    }

    // --- Combinations ---
    for i in 1 ..= list.len() {
        let combinator = LargeCombinationIterator::new(&list, i);
        for c in combinator {
            let total: usize = c.iter().copied().sum();
            if total == LITERS {
                count += 1;
                *map.entry(c.len()).or_insert(0) += 1;
            }
        }
    }

    // --- Puzzle 1 ---
    println!("1. Combinations of containers: {}", count);

    // --- Puzzle 2 ---
    let min = map.iter().map(|(k, _)| k).min().unwrap();
    println!("2. Number of different ways: {}", map[min]);

    Ok(())
}
