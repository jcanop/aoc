pub mod item;
pub mod tokenizer;

use item::Item;
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut list: Vec<Item> = vec![];
    let mut total = 0;

    let file = File::open(INPUT_FILE)?;
    let mut index: usize = 0;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        let id = index / 3 + 1;
        match index % 3 {
            0 => list.push(line.parse::<Item>()?),
            1 => {
                list.push(line.parse::<Item>()?);
                let a = &list[list.len() - 2];
                let b = &list[list.len() - 1];
                if a < b { total += id; }
            },
            2 => (),
            _ => unreachable!()
        }
        index += 1;
    }

    // --- Puzzle 1 ---
    println!("1. The sum of the indices of the right order pairs: {}", total);

    // --- Puzzle 2 ---
    list.push("[[2]]".parse().unwrap());
    list.push("[[6]]".parse().unwrap());
    list.sort();
    let index1 = list.iter().position(|item| item.to_string() == "[[2]]").unwrap() + 1;
    let index2 = list.iter().position(|item| item.to_string() == "[[6]]").unwrap() + 1;
    total = index1 * index2;
    println!("2. The decoder key is: {}", total);

    Ok(())
}
