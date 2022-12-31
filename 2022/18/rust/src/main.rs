use rust::*;
use std::collections::HashSet;
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants ---
const INPUT_FILE: &'static str = "../input/input.txt";

fn main() -> std::io::Result<()>  {
    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    let mut list: Vec<Point> = BufReader::new(file).lines().map(|line| {
            let a: Vec<usize> = line.unwrap().split(",")
                .map(|n| n.parse::<usize>().unwrap())
                .collect();
            (a[0], a[1], a[2])
        }).collect();

    // --- Search for the max values ---
    let mut max: [usize; 3] = [0; 3];
    max[0] = list.iter().map(|p| p.0).max().unwrap();
    max[1] = list.iter().map(|p| p.1).max().unwrap();
    max[2] = list.iter().map(|p| p.2).max().unwrap();

    // --- Puzzle 1 ---
    let total = calculate(&list, &max);
    println!("1. Total surface: {}", total);

    // --- Puzzle 2 ---
    let mut closed: HashSet<Point> = HashSet::new();
    for x in 0..max[0] {
        for y in 0..max[1] {
            for z in 0..max[2] {
                let p = (x, y , z);
                if list.contains(&p) { continue; }
                if !closed.contains(&p) { dfs(&p, &list, &max, &mut closed); }
            }
        }
    }

    for p in closed.into_iter() {
        list.push(p);
    }

    let total = calculate(&list, &max);
    println!("2. Adjusted total surface: {}", total);

    Ok(())
}
