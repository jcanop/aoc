use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";

type Grid = Vec<Vec<usize>>;

// Checks if a tree is visible.
fn is_visible(grid: &Grid, x: usize, y: usize) -> bool {
    for i in y + 1 .. grid.len() {
        if grid[i][x] >= grid[y][x] { break; }
        if i == grid.len() - 1 { return true; }
    }
    for i in (0 ..= y - 1).rev() {
        if grid[i][x] >= grid[y][x] { break; }
        if i == 0 { return true; }
    }
    for i in x + 1 .. grid[0].len() {
        if grid[y][i] >= grid[y][x] { break; }
        if i == grid[0].len() - 1 { return true; }
    }
    for i in (0 ..= x - 1).rev() {
        if grid[y][i] >= grid[y][x] { return false; }
        if i == 0 { return true; }
    }
    unreachable!();
}

// Calculate scenic score of a tree.
fn scenic_score(grid: &Grid, x: usize, y: usize) -> usize {
    let mut a = 1;
    for i in y + 1 .. grid.len() - 1 {
        if grid[i][x] >= grid[y][x] { break; }
        a += 1;
    }
    let mut b = 1;
    for i in (1 ..= y - 1).rev() {
        if grid[i][x] >= grid[y][x] { break; }
        b += 1;
    }
    let mut c = 1;
    for i in x + 1 .. grid[0].len() - 1 {
        if grid[y][i] >= grid[y][x] { break; }
        c += 1;
    }
    let mut d = 1;
    for i in (1 ..= x - 1).rev() {
        if grid[y][i] >= grid[y][x] { break; }
        d += 1;
    }
    a * b * c * d
}

fn main() -> Result<(), std::io::Error> {
    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    let grid: Grid = BufReader::new(file).lines().map(|line| {
        let line = line.unwrap();
        line.chars().map(|c| c.to_digit(10).unwrap() as usize).collect()
    }).collect();

    // --- Puzzle 1 ---
    let mut total = (grid.len() + grid[0].len() - 2) * 2;
    for y in 1 .. grid.len() - 1 {
        for x in 1 .. grid[0].len() - 1 {
            total += if is_visible(&grid, x, y) { 1 } else { 0 }
        }
    }
    println!("1. Trees that are visible from aoutside the grid: {}", total);

    // --- Puzzle 2 ---
    let mut max = 0;
    for y in 1 .. grid.len() - 1 {
        for x in 1 .. grid[0].len() - 1 {
            let res = scenic_score(&grid, x, y);
            if res > max { max = res; }
        }
    }
    println!("2. The highest scenic score is:  {}", max);

    Ok(())
}
