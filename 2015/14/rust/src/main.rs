use regex::Regex;
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = r"^.+ can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.$";
const TIME: usize = 2503;

// --- Structs ---
#[derive(Debug)]
struct Reindeer {
    speed: usize,
    fly: usize,
    rest: usize,
    distance: usize,
    fly_count: usize,
    rest_count: usize,
    points: usize
}

impl Reindeer {
    fn new(speed: usize, fly: usize, rest: usize) -> Self {
        Reindeer { speed, fly, rest, distance: 0, fly_count: fly, rest_count: rest, points: 0 }
    }

    fn tick(&mut self) {
        if self.fly_count > 0 {
            self.fly_count -= 1;
            self.distance += self.speed;
        } else if self.rest_count > 0 {
            self.rest_count -= 1;
        } else {
            self.fly_count = self.fly - 1;
            self.rest_count = self.rest;
            self.distance += self.speed;
        }
    }
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let regex = Regex::new(PARSER_REGEX)?;
    let mut reindeers: Vec<Reindeer> = Vec::new();
    let mut max = std::usize::MIN;

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        for cap in regex.captures_iter(&line) {
            let v: usize = cap[1].parse()?;
            let f: usize = cap[2].parse()?;
            let r: usize = cap[3].parse()?;
            reindeers.push(Reindeer::new(v, f, r));

            // --- Puzzle 1 ---
            let d = TIME / (f + r) * v * f + v * std::cmp::min(f, TIME % (f + r));
            max =  std::cmp::max(max, d);
        }
    }

    // --- Puzzle 1 ---
    println!("1. Winning reindeer distance: {} km", max);

    // --- Puzzle 2 ---
    for _ in 0 .. TIME {
        reindeers.iter_mut().for_each(|r| r.tick());
        max = reindeers.iter().max_by_key(|r| r.distance).unwrap().distance;
        reindeers.iter_mut().for_each(|r| if r.distance == max { r.points += 1; });
    }
    max = reindeers.iter().max_by_key(|r| r.points).unwrap().points;
    println!("2. Winning reindeer points: {}", max);

    Ok(())
}
