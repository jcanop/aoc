use std::fs::File;
use std::io::{ BufRead, BufReader };

const INPUT_FILE:&'static str = "../input/input.txt";

fn main() -> Result<(), std::io::Error> {
    let mut list:Vec<u64> = Vec::new();
    let mut total: u64 = 0;

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        if line.len() == 0 {
            list.push(total);
            total = 0;
        } else {
            total += line.parse::<u64>().unwrap();
        }
    }
    list.sort();
    list.reverse();

    // --- Find the Elf carrying the most Calories ---
    let max = list[0];
    println!("The Elf carrying the most Calories, is carrying {} Calories.", max);

    // --- Find the top three Elves carrying the most Calories ---
    total = list[0..3].iter().sum();
    println!("The top 3 Elves carrying the most Calores, are carrying {} Calories.", total);
    Ok(())
}
