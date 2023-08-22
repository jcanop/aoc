use permutator::HeapPermutationIterator;
use regex::Regex;
use std::collections::{ HashMap, HashSet };
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = r"^(.+) to (.+) = (\d+)$";

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let regex = Regex::new(PARSER_REGEX)?;

    let mut places: HashSet<String> = HashSet::new();
    let mut distances: HashMap<(String, String), u32> = HashMap::new();
    let mut min_distance = std::u32::MAX;
    let mut max_distance = std::u32::MIN;

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        for cap in regex.captures_iter(&line) {
            places.insert(cap[1].to_string());
            places.insert(cap[2].to_string());
            distances.insert((cap[1].to_string(), cap[2].to_string()), cap[3].parse()?);
            distances.insert((cap[2].to_string(), cap[1].to_string()), cap[3].parse()?);
        }
    }

    // --- Calculate distances ---
    let mut places: Vec<String> = places.into_iter().collect();
    let permutator = HeapPermutationIterator::new(&mut places);
    for p in permutator {
        let mut distance = 0;
        for i in 0 .. p.len() - 1 {
            let key = (p[i].to_string(), p[i + 1].to_string());
            distance += distances.get(&key).unwrap();
        }

        if distance < min_distance { min_distance = distance; }
        if distance > max_distance { max_distance = distance; }
    }

    // --- Results --
    println!("1. Min distance: {}", min_distance);
    println!("2. Max distance: {}", max_distance);

    Ok(())
}
