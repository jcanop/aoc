use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";

fn count_mem(s: &str) -> usize {
    let mut total = s.len() - 2;
    let a: Vec<_> = s.chars().collect();
    let mut i = 1;
    while i < a.len() - 2 {
        if a[i] == '\\' {
            if a[i + 1] == '\\' || a[i + 1] == '"' {
                total -= 1;
                i += 1;
            } else if a[i + 1] == 'x' {
                total -= 3;
                i += 3;
            }
        }
        i += 1;
    }
    total
}

fn encode(s: &str) -> String {
    let mut v: Vec<char> = Vec::new();
    v.push('"');
    for c in s.chars() {
        if c == '"' || c == '\\' {
            v.push('\\');
        }
        v.push(c);
    }
    v.push('"');
    v.into_iter().collect()
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut total1 = 0;
    let mut total2 = 0;

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        total1 += line.len() - count_mem(&line);

        let enc = encode(&line);
        total2 += enc.len() - count_mem(&enc);
    }

    // --- Puzzle 1 ---
    println!("1. Number of characters in memory: {}", total1);

    // --- Puzzle 2 ---
    println!("2. Number of characters in memory: {}", total2);

    Ok(())
}
