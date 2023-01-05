use crate::{ Blizzard, Grid, Point };
use std::fs::File;
use std::io::{ BufRead, BufReader };

pub struct Map {
    blizzard: Blizzard,
    data: Grid,
    next: Grid,
    pub height: usize,
    pub width: usize
}

impl Map {
    // --- Loads an input file and creats a map ---
    pub fn load(filename: &str) -> std::io::Result<Self> {
        let mut blizzard = Blizzard::new();

        let file = File::open(filename)?;
        let lines: Vec<String> = BufReader::new(file).lines().map(|x| x.unwrap()).collect();
        let height = lines.len();
        let width = lines[0].len();
        for y in 1..height - 1 {
            blizzard.add(&lines[y][1..width - 1]);
        }
        let data = vec![vec![' '; width]; height];
        let next = vec![vec![' '; width]; height];

        Ok(Self{ blizzard, data, next, height, width })
    }

    // --- checks if you can move to a space at a time ---
    fn can_move(&self, x: usize, y: usize, time: usize) -> bool {
        if x == 1 && y == 0 { return true; } // Start position
        if x == self.width - 2 && y == self.height - 1 { return true; } // End position
        if x < 1 || x >= self.width - 1 || y < 1 || y >= self.height - 1 { return false; } // Border
        return self.blizzard.is_empty(x, y, time)
    }

    // --- Search the fastest path between two points ---
    pub fn search(&mut self, start: &Point, end: &Point, time: usize) -> usize {
        let mut time = time;
        for y in 0..self.height {
            for x in 0..self.width {
                self.data[y][x] = ' ';
            }
        }
        self.data[start.1][start.0] = 'X';

        while self.data[end.1][end.0] != 'X' {
            for y in 0..self.height {
                for x in 0..self.width {
                    self.next[y][x] = ' ';
                }
            }
            for y in 0..self.height {
                for x in 0..self.width {
                    if self.data[y][x] == 'X' {
                        if                        self.can_move(x    , y, time + 1) { self.next[y][x]     = 'X'; }
                        if x > 0               && self.can_move(x - 1, y, time + 1) { self.next[y][x - 1] = 'X'; }
                        if x < self.width - 1  && self.can_move(x + 1, y, time + 1) { self.next[y][x + 1] = 'X'; }
                        if y > 0               && self.can_move(x, y - 1, time + 1) { self.next[y - 1][x] = 'X'; }
                        if y < self.height - 1 && self.can_move(x, y + 1, time + 1) { self.next[y + 1][x] = 'X'; }
                    }
                }
            }
            time += 1;
            for y in 0..self.height {
                for x in 0..self.width {
                    self.data[y][x] = self.next[y][x];
                }
            }
        }
        time
    }
}
