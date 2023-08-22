use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";
const LIST_1: [char; 5] = ['a', 'e', 'i', 'o', 'u'];
const LIST_2: [&'static str; 4] = ["ab", "cd", "pq", "xy"];

// --- Rules ---
fn rule_1_1(s: &str) -> bool {
    let mut count = 0;
    for c in s.chars() {
       if LIST_1.contains(&c) { count += 1; }
       if count == 3 { return true; }
    }
    return false;
}

fn rule_1_2(s: &str) -> bool {
    let a:Vec<_> = s.chars().collect();
    for i in 0 .. a.len() - 1 {
        if a[i] == a[i + 1] { return true; }
    }
    return false;
}

fn rule_1_3(s: &str) -> bool {
    for c in LIST_2 {
        if s.contains(c) { return false; }
    }
    return true;
}

fn rule_2_1(s: &str) -> bool {
    let a:Vec<_> = s.chars().collect();
    for i in 0 .. a.len() - 2 {
        for j in i + 2 .. a.len() - 1 {
            if a[i] == a[j] && a[i + 1] == a[j + 1] { return true; }
        }
    }
    return false;
}

fn rule_2_2(s: &str) -> bool {
    let a:Vec<_> = s.chars().collect();
    for i in 0 .. a.len() - 2 {
        if a[i] == a[i + 2] { return true; }
    }
    return false;
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut count1 = 0;
    let mut count2 = 0;

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        if rule_1_1(&line) && rule_1_2(&line) && rule_1_3(&line) { count1 += 1; }
        if rule_2_1(&line) && rule_2_2(&line) { count2 += 1; }
    }

    // --- Puzzle 1 ---
    println!("1. Nice Strings: {}", count1);

    // --- Puzzle 2 ---
    println!("2. Nice Strings: {}", count2);

    Ok(())
}
