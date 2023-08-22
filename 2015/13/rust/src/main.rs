use itertools::Itertools;
use regex::Regex;
use std::collections::{ HashMap, HashSet };
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = r"(.+)? would (gain|lose) (\d+) happiness units by sitting next to (.+)\.";

// --- Calculate happiness ---
fn calculate_happiness(persons: &Vec<String>, happiness: &HashMap<(String, String), isize>) -> isize {
    let mut max = std::isize::MIN;
    for c in persons.iter().permutations(persons.len()).unique() {
        let mut h = 0;
        for i in 0 .. c.len() {
            let n1 = c[i];
            let n2 = c[(i + 1) % c.len()];
            h += happiness.get(&(n1.to_string(), n2.to_string())).unwrap();
            h += happiness.get(&(n2.to_string(), n1.to_string())).unwrap();
        }
        max = std::cmp::max(max, h);
    }
    max
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let regex = Regex::new(PARSER_REGEX)?;

    let mut persons: HashSet<String> = HashSet::new();
    let mut happiness: HashMap<(String, String), isize> = HashMap::new();

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        for cap in regex.captures_iter(&line) {
            persons.insert(cap[1].to_string());
            persons.insert(cap[4].to_string());
            let mut h: isize = cap[3].parse()?;
            match &cap[2] {
                "gain" => (),
                "lose" => h *= -1,
                _ => panic!("Invalid option")
            }
            happiness.insert((cap[1].to_string(), cap[4].to_string()), h);
        }
    }
    let mut persons: Vec<String> = persons.into_iter().collect();

    // --- Puzzle 1 ---
    let max = calculate_happiness(&persons, &happiness);
    println!("1. Total change in happiness: {}", max);

    // --- Puzzle 2 ---
    persons.push("me".to_string());
    for p in &persons {
        happiness.insert((p.to_string(), "me".to_string()), 0);
        happiness.insert(("me".to_string(), p.to_string()), 0);
    }
    let max = calculate_happiness(&persons, &happiness);
    println!("2. Total change in happiness: {}", max);

    Ok(())
}
