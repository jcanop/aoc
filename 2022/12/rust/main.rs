use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";

// --- Types ---
type Point = (usize, usize);
type Grid = Vec<Vec<char>>;

// --- Structs ---
struct Map {
    grid: Grid,
    start: (usize, usize),
    end: (usize, usize)
}

impl Map {
    fn load_from_file(filename: &str) -> Result<Self, std::io::Error> {
        let file = File::open(filename)?;
        let mut grid: Grid = BufReader::new(file).lines()
            .map(|line| line.unwrap().chars().collect())
            .collect();
        let mut start: Option<Point> = None;
        let mut end: Option<Point> = None;
        for y in 0..grid.len() {
            for x in 0..grid[0].len() {
                if grid[y][x] == 'S' { start = Some((x, y)); grid[y][x] = 'a'; }
                if grid[y][x] == 'E' { end   = Some((x, y)); grid[y][x] = 'z'; }
            }
        }
        if start.is_none() || end.is_none() {
            panic!("Missing Starting or Ending point");
        }
        Ok(Map { grid, start: start.unwrap(), end: end.unwrap() })
    }

    fn find_path(&self) -> Vec<Point> {
        let h = self.grid.len();
        let w = self.grid[0].len();
        let x = self.start.0;
        let y = self.start.1;
        let mut a: Vec<Vec<usize>> = vec![vec![0; w]; h];
        a[y][x] = 1;
        let mut list: Vec<Point> = vec![ self.start ];
        while list.len() > 0 {
            let p = list.remove(0);
            let x = p.0;
            let y = p.1;
            if x > 0     && a[y][x - 1] == 0 && self.grid[y][x - 1] as usize <= self.grid[y][x] as usize + 1 { a[y][x - 1] = a[y][x] + 1; list.push((x - 1, y)); }
            if y > 0     && a[y - 1][x] == 0 && self.grid[y - 1][x] as usize <= self.grid[y][x] as usize + 1 { a[y - 1][x] = a[y][x] + 1; list.push((x, y - 1)); }
            if x < w - 1 && a[y][x + 1] == 0 && self.grid[y][x + 1] as usize <= self.grid[y][x] as usize + 1 { a[y][x + 1] = a[y][x] + 1; list.push((x + 1, y)); }
            if y < h - 1 && a[y + 1][x] == 0 && self.grid[y + 1][x] as usize <= self.grid[y][x] as usize + 1 { a[y + 1][x] = a[y][x] + 1; list.push((x, y + 1)); }
        }

        let mut x = self.end.0;
        let mut y = self.end.1;
        let mut c = a[y][x];
        list.clear();
        while c > 1 {
            if x > 0 && a[y][x - 1] == c { x -= 1; }
            else if y > 0 && a[y - 1][x] == c { y -= 1; }
            else if x < w - 1 && a[y][x + 1] == c { x += 1; }
            else if y < h - 1 && a[y + 1][x] == c { y += 1; }
            list.push((x, y));
            c -= 1;
        }
        list
    }

    fn find_all_paths(&mut self) -> Vec<Vec<Point>> {
        let h = self.grid.len();
        let w = self.grid[0].len();
        let mut list: Vec<Vec<Point>> = vec![];
        for y in 0..h {
            for x in 0..w {
                if self.grid[y][x] == 'a' {
                    self.start = (x, y);
                    let path = self.find_path();
                    if path.len() > 0 { list.push(path); }
                }
            }
        }
        list
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Parse the input file ---
    let mut map = Map::load_from_file(INPUT_FILE)?;

    // --- Puzzle 1 ---
    let count = map.find_path().len();
    println!("1. Fewest steps: {}", count);

    // --- Puzzle 2 ---
    let min = map.find_all_paths().iter().map(|x| x.len()).min().unwrap();
    println!("2. Shortest path: {}", min);

    Ok(())
}
