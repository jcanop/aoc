use regex::Regex;
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants ---
const INPUT_FILE: &'static str = "../input/input.txt";
const LEN: usize = 1_000;
const PARSER_REGEX: &'static str = "(?:(toggle|turn on|turn off)) (?:(\\d+)),(?:(\\d+)) through (?:(\\d+)),(?:(\\d+))";

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let regex = Regex::new(PARSER_REGEX)?;
    let mut map1 = [[false; LEN]; LEN];
    let mut map2 = [[0u32; LEN]; LEN];

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        for cap in regex.captures_iter(&line) {
            let op = &cap[1];
            let x1: usize = cap[2].parse()?;
            let y1: usize = cap[3].parse()?;
            let x2: usize = cap[4].parse()?;
            let y2: usize = cap[5].parse()?;

            for y in y1 .. y2 + 1 {
                for x in x1 .. x2 + 1 {
                    match op {
                        "toggle"   => { map1[x][y] = !map1[x][y]; map2[x][y] += 2; },
                        "turn on"  => { map1[x][y] = true; map2[x][y] += 1; },
                        "turn off" => { map1[x][y] = false; if map2[x][y] > 0 { map2[x][y] -= 1; }},
                        _          => panic!("Unsupported op")
                    }
                }
            }
        }
    }

    // --- Puzzle 1 ---
    let mut count = 0;
    for y in 0 .. LEN {
        for x in 0 .. LEN {
            if map1[x][y] { count += 1 }
        }
    }
    println!("1. Lights lit: {}", count);

    // --- Puzzle 2 ---
    count = 0;
    for y in 0 .. LEN {
        for x in 0 .. LEN {
            count += map2[x][y];
        }
    }
    println!("2. Total brightness: {}", count);

    Ok(())
}
