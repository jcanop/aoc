use std::fs::File;
use std::io::{ BufRead, BufReader };
use std::num::ParseIntError;
use std::str::FromStr;

const INPUT_FILE:&'static str = "../input/input.txt";

// Private struct that represents a range.
#[derive(Debug)]
struct Range {
    min: usize,
    max: usize
}

impl Range {
    // Checks if this range fully contains another range.
    pub fn fully_contains(&self, r: &Range) -> bool {
        self.min <= r.min && r.max <= self.max
    }

    // Checks if this range overlaps with another range.
    pub fn overlap(&self, r: &Range) -> bool {
        (r.min >= self.min && r.min <= self.max) ||
        (r.max >= self.min && r.max <= self.max) ||
        (self.min >= r.min && self.min <= r.max) ||
        (self.max >= r.min && self.max <= r.max)
    }
}

impl FromStr for Range {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let i = s.find("-").unwrap();
        let min = s[0..i].parse::<usize>()?;
        let max = s[i + 1..].parse::<usize>()?;
        Ok(Range { min, max })
    }
}

fn main() -> Result<(), std::io::Error> {
    // --- Parse input file ---
    let file = File::open(INPUT_FILE)?;
    let list: Vec<(Range, Range)> = BufReader::new(file).lines().map(|line| {
        let line = line.unwrap();
        let i = line.find(",").unwrap();
        let r1 = line[0..i].parse::<Range>().unwrap();
        let r2 = line[i + 1..].parse::<Range>().unwrap();
        ( r1, r2 )
    }).collect();

    // --- Puzzle: Part 1 ---
    let total: usize = list.iter().map(|(r1, r2)| {
            if r1.fully_contains(r2) || r2.fully_contains(r1) { 1 } else { 0 }
        }).sum();
    println!("Part 1. Total of ranges that fully contains another: {}", total);

    // --- Puzzle: Part 2 ---
    let total: usize = list.iter().map(|(r1, r2)| {
            if r1.overlap(r2) { 1 } else { 0 }
        }).sum();
    println!("Part 2. Total of ranges that overlaps: {}", total);

    Ok(())
}
