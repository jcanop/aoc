use rand::thread_rng;
use rand::seq::SliceRandom;
use regex::Regex;
use std::collections::{ HashMap, HashSet };
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = r"^([a-zA-Z]+) => ([a-zA-Z]+)$";

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let regex = Regex::new(PARSER_REGEX)?;
    let mut map: HashMap<String, Vec<String>> = HashMap::new();
    let mut pairs: Vec<(String, String)> = Vec::new();
    let mut molecule = String::new();

    // --- Read and parse the input file ---
    let mut first = true;
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        if line.len() == 0 {
            first = false;
            continue;
        }

        if first {
            let cap = regex.captures(&line).unwrap();
            map.entry(cap[1].to_string()).or_insert(Vec::new()).push(cap[2].to_string());
            pairs.push((cap[1].to_string(), cap[2].to_string()));
        } else {
            molecule = line.to_string();
        }
    }

    // --- Puzzle 1 ---
    let mut list: Vec<char> = vec!['('];
    for s in map.keys() {
        list.extend(s.chars());
        list.push('|');
    }
    let i = list.len() - 1;
    list[i] = ')';
    let parser_regex = list.into_iter().collect::<String>();
    let regex = Regex::new(&parser_regex)?;

    let mut set: HashSet<String> = HashSet::new();
    for cap in regex.captures_iter(&molecule) {
        let m = cap.get(1).unwrap();
        let i = m.start();
        let j = m.end();
        for x in map[&cap[1]].iter() {
            let s = format!("{}{}{}", &molecule[0..i], x, &molecule[j..]);
            set.insert(s);
        }
    }
    println!("1. Distinct molecules can be created: {}", set.len());

    // --- Puzzle 2 ---
    let mut target = molecule.clone();
    let mut steps = 0;
    while target != "e" {
        let mut change = false;
        for (from, to) in &pairs {
            if target.contains(to) {
                target = target.replacen(to, from, 1);
                change = true;
                break;
            }
        }
        // --- If not change was possible shuffle pairs and try again ---
        if !change {
            pairs.shuffle(&mut thread_rng());
            target = molecule.clone();
            steps = 0;
            continue;
        }
        steps += 1;
    }
    println!("2. Fewest number of steps: {}", steps);

    Ok(())
}
