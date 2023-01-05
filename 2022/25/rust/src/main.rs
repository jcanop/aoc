use rust::snafu;
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants ---
const INPUT_FILE: &'static str = "../input/input.txt";

// --- Main function ---
fn main() -> std::io::Result<()> {
    let file = File::open(INPUT_FILE)?;
    let total = BufReader::new(file).lines().map(|s| snafu::from(&s.unwrap())).sum();
    let snafu = snafu::to(total);
    println!("SNAFU number to supply to Bob's console: {}", snafu);
    Ok(())
}
