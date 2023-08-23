use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants ---
const INPUT_FILE: &'static str = "../input/input.txt";
const KEY: i64 = 811589153;
const MIX_COUNT: usize = 10;
const OFFSET_X: usize = 1_000;
const OFFSET_Y: usize = 2_000;
const OFFSET_Z: usize = 3_000;

#[derive(PartialEq, Clone)]
struct Item(usize, i64);

// --- Mix the order of numbers ---
fn mix (list: &mut Vec<Item>) {
    let len = (list.len() - 1) as i64;
    for id in 0..list.len() {
        let i = list.iter().position(|x| x.0 == id).unwrap();
        if list[i].1 == 0 { continue; }
        let item = list.remove(i);
        let mut j = (i as i64 + item.1) % len;
        if j < 0 { j = len + j; }
        if j == 0 { j = len; }
        list.insert(j as usize, item);
    }
}

// --- Find and sum the 3 coordinates ---
fn sum(list: &Vec<Item>) -> i64 {
    let i = list.iter().position(|x| x.1 == 0).unwrap();
    let len = list.len();
    let x: i64 = list[(i + OFFSET_X) % len].1 as i64;
    let y: i64 = list[(i + OFFSET_Y) % len].1 as i64;
    let z: i64 = list[(i + OFFSET_Z) % len].1 as i64;
    x + y + z
}

// --- Main method ---
fn main() -> std::io::Result<()> {
    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    let vec: Vec<i64> = BufReader::new(file).lines()
        .map(|line| line.unwrap().parse::<i64>().unwrap())
        .collect();

    // --- Puzzle 1 ---
    let mut list: Vec<Item> = vec.iter().enumerate().map(|(i, x)| Item(i, *x)).collect();
    mix(&mut list);
    let total = sum(&list);
    println!("1. The sum of the coordiantes: {}", total);

    // --- Puzzle 2 ---
    let mut list:Vec<Item> = vec.into_iter().enumerate().map(|(i, x)| Item(i, x * KEY))
        .collect();
    for _ in 0..MIX_COUNT {
        mix(&mut list);
    }
    let total = sum(&list);
    println!("2. The sum of the coordinates: {}", total);

    Ok(())
}
