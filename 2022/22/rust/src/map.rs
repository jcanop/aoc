use crate::{ EAST, LEFT, RIGHT, EMPTY, OPEN, Grid };
use regex::Regex;

pub struct Map {
    pub grid: Grid,
    pub height: usize,
    pub width: usize,
    pub x: usize,
    pub y: usize,
    pub d: usize
}

impl Map {
    pub fn load(data: &str) -> Self {
        let mut grid: Vec<Vec<char>> = data.split("\n")
            .map(|line| line.chars().collect()).collect();
        let max = grid.iter().map(|r| r.len()).max().unwrap();
        for row in grid.iter_mut() {
            for _ in row.len()..max { row.push(EMPTY); }
        }
        let height = grid.len();
        let width = grid[0].len();
        let (x, y) = if let (y, Some((x, _))) = grid.iter()
            .map(|r| r.iter().enumerate().find(|(_, c)| c == &&OPEN))
            .enumerate().find(|(_, c)| c.is_some()).unwrap() { (x, y) } else { panic!() };
        let d = EAST;
        Self { grid, height, width, x, y, d }
    }

    pub fn path(&mut self, path: &str, move_steps: fn(map: &mut Self, steps: usize) -> ()) {
        let regex = Regex::new(r"(\d+|R|L)").unwrap();
        for cap in regex.captures_iter(path) {
            let c = cap[1].chars().nth(0).unwrap();
            match c {
                LEFT  => if self.d == 0 { self.d = 3; } else { self.d -= 1; },
                RIGHT => if self.d == 3 { self.d = 0; } else { self.d += 1; },
                _ => {
                    let steps = cap[1].parse().unwrap();
                    move_steps(self, steps);
                }
            }
        }
    }

    pub fn get_password(&self) -> usize {
        1_000 * (self.y + 1) + 4 * (self.x + 1) + self.d
    }
}
