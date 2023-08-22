use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = r"([a-z]+): (\d+)";

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let regex = Regex::new(PARSER_REGEX)?;

    // --- MFCSAM Output ---
    let mut map: HashMap<String, usize> = HashMap::new();
    map.insert("children".to_string(), 3);
    map.insert("cats".to_string(), 7);
    map.insert("samoyeds".to_string(), 2);
    map.insert("pomeranians".to_string(), 3);
    map.insert("akitas".to_string(), 0);
    map.insert("vizslas".to_string(), 0);
    map.insert("goldfish".to_string(), 5);
    map.insert("trees".to_string(), 3);
    map.insert("cars".to_string(), 2);
    map.insert("perfumes".to_string(), 1);

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    'outer: for line in BufReader::new(file).lines() {
        let line = line.unwrap();

        // --- Puzzle 1 ---
        for cap in regex.captures_iter(&line) {
            let key = cap[1].to_string();
            let value = cap[2].parse::<usize>()?;

            if let Some(v) = map.get(&key) {
                if *v != value { continue 'outer; }
            }
        }
        let sue = line.split(":").next().unwrap();
        println!("1. Number of the Sue that got you the gift: {}", sue);
        break;
    }

    let file = File::open(INPUT_FILE)?;
    'outer: for line in BufReader::new(file).lines() {
        let line = line.unwrap();

        // --- Puzzle 2 ---
        for cap in regex.captures_iter(&line) {
            let key = cap[1].to_string();
            let value = cap[2].parse::<usize>()?;

            if let Some(v) = map.get(&key) {
                if key == "cats" || key == "trees" {
                    if *v >= value { continue 'outer; }
                } else if key == "pomeranians" || key == "goldfish" {
                    if *v <= value { continue 'outer; }
                } else {
                    if *v != value { continue 'outer; }
                }
            }
        }
        let sue = line.split(":").next().unwrap();
        println!("2. Number of the Sue that got you the gift: {}", sue);
        break;
    }

    Ok(())
}
