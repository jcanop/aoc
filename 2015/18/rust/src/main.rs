use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const STEPS: usize = 100;
const ON:char = '#';
const OFF:char = '.';

// --- Types ---
type Grid = Vec<Vec<char>>;

// --- Count on neighbors ---
fn count_on_neighbors(grid: &Grid, x: usize, y: usize) -> usize {
    if grid.len() == 0 { return 0; }
    let mut c = 0;
    for j in 0 .. 3 {
        if j == 0 && y == 0 { continue; }
        if j == 2 && y == grid.len() - 1 { continue; }
        let ny = y + j - 1;
        for i in 0 .. 3 {
            if i == 1 && j == 1 { continue; }
            if i == 0 && x == 0 { continue; }
            if i == 2 && x == grid[0].len() - 1 { continue; }
            let nx = x + i - 1;
            if grid[ny][nx] == ON {
                c += 1;
            }
        }
    }
    c
}

// --- Animation for puzzle 1 and 2 ---
fn animation(data: &Grid, puzzle: usize) -> usize {
    // --- Init the two grids ---
    let mut grid1: Grid = Vec::new();
    let mut grid2: Grid = Vec::new();
    for row in data {
        grid1.push(row.clone());
        grid2.push(row.clone());
    }
    let x_len = data[0].len();
    let y_len = data.len();

    // --- Run the animation ---
    let mut grid = &mut grid1;
    let mut buffer = &mut grid2;
    if puzzle == 2 {
        grid[0][0] = ON;
        grid[0][x_len - 1] = ON;
        grid[y_len - 1][0] = ON;
        grid[y_len - 1][x_len - 1] = ON;
    }
    for step in 0 .. STEPS {
        for y in 0 .. grid.len() {
            for x in 0 .. grid[0].len() {
                if puzzle == 2 {
                    if  (x == 0 && y == 0) ||
                        (x == 0 && y == y_len - 1) ||
                        (x == x_len - 1 && y == 0) ||
                        (x == x_len - 1 && y == y_len - 1) {
                        buffer[y][x] = ON;
                        continue;
                    }
                }
                let c = count_on_neighbors(&grid, x, y);
                if grid[y][x] == ON {
                    buffer[y][x] = if c == 2 || c == 3 { ON } else { OFF };
                } else {
                    buffer[y][x] = if c == 3 { ON } else { OFF };
                }
            }
        }
        if step % 2 == 0 {
            grid = &mut grid2;
            buffer = &mut grid1;
        } else {
            grid = &mut grid1;
            buffer = &mut grid2;
        }
    }

    // --- Return lights on ---
    grid.iter().map(|v| v.iter().map(|x| if *x == ON { 1 } else { 0 }).sum::<usize>()).sum()
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut data: Grid = Vec::new();

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        data.push(line.chars().collect());
    }

    // --- Puzzle 1 ---
    let c = animation(&data, 1);
    println!("1. Number of lights on: {}", c);

    // --- Puzzle 2 ---
    let c = animation(&data, 2);
    println!("2. Number of lights on: {}", c);

    Ok(())
}
