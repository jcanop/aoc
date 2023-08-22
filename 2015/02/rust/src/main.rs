use regex::Regex;
use std::cmp::{ max, min };
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = "(?:(\\d+))x(?:(\\d+))x(?:(\\d+))";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let regex = Regex::new(PARSER_REGEX)?;
    let mut total: usize = 0;
    let mut ribbon: usize = 0;

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
//println!("{}", &line);
        for cap in regex.captures_iter(&line) {
            let l: usize = cap[1].parse()?;
            let w: usize = cap[2].parse()?;
            let h: usize = cap[3].parse()?;
            let a = l * w;
            let b = w * h;
            let c = l * h;
            let m = min(a, min(b, c));
            total += 2 * a + 2 * b + 2 * c + m;
//println!("{}x{}x{} - {}x{}x{}x{} - {}", l, w, h, a, b, c, m, total);

            let m = max(l, max(w, h));
            ribbon += 2 * (l + w + h - m) + l * w * h;
        }
    }

    // --- Puzzle 1 ---
    println!("1. Total square feet of wrapping paper: {}", total);

    // --- Puzzle 2 ---
    println!("2. Total feet of ribbon: {}", ribbon);

    Ok(())
}
