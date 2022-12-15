use std::collections::HashMap;
use std::fmt::{ Display, Formatter };
use std::fs::File;
use std::io::{ BufRead, BufReader };
use map::Tile;

// --- Constants ---
const OFFSET: usize = 2;
const ORIGIN: Point = (500, 0);

// --- Types ---
type Point = (usize, usize);
type Data = HashMap<Point, Tile>;

// --- Structs ---
pub struct Map {
    data: Data,
    height: usize
}

impl Map {
    pub fn load(filename: &str) -> Result<Self, std::io::Error> {
        let mut list: Vec<Vec<Point>> = vec![];
        let mut maxy = 0;
        let file = File::open(filename)?;
        for line in BufReader::new(file).lines() {
            let line = line.unwrap();
            let ln: Vec<Point> = line.split(" -> ").map(|token| {
                let s: Vec<&str> = token.split(",").collect();
                let x: usize = s[0].parse().unwrap();
                let y: usize = s[1].parse().unwrap();
                (x, y)
            }).collect();
            let max = ln.iter().map(|(_, y)| y).max().unwrap();
            maxy = std::cmp::max(maxy, *max);
            list.push(ln);
        }

        let mut data: Data = HashMap::new();
        let height = maxy + 3;
        for ln in list {
            for i in 1..ln.len() {
                let (px1, py1) = ln.get(i - 1).unwrap();
                let (px2, py2) = ln.get(i).unwrap();
                let x1 = *std::cmp::min(px1, px2);
                let x2 = *std::cmp::max(px1, px2);
                let y1 = *std::cmp::min(py1, py2);
                let y2 = *std::cmp::max(py1, py2);
                for y in y1..=y2 {
                    for x in x1..=x2 {
                        data.insert((x, y), Tile::Rock);
                    }
                }
            }
        }

        Ok(Map { data, height })
    }

    fn get(&self, x: usize, y: usize) -> &Tile {
        if y == self.height - 1 { return &Tile::Rock; }
        if let Some(tile) = self.data.get(&(x, y)) {
            return tile;
        }
        if ORIGIN.0 == x && ORIGIN.1 == y { return &Tile::Origin; }
        &Tile::Air
    }

    pub fn simulate(&mut self, puzzle: usize) {
        loop {
            let mut x = ORIGIN.0;
            let mut y = ORIGIN.1;
            loop {
                if puzzle == 1 && y + 1 == self.height - 1 { return (); }
                if self.get(x, y + 1) == &Tile::Air { y += 1; continue; }
                if self.get(x - 1, y + 1) == &Tile::Air { x -= 1; y += 1; continue; }
                if self.get(x + 1, y + 1) == &Tile::Air { x += 1; y += 1; continue; }
                self.data.insert((x, y), Tile::Sand);
                if ORIGIN.0 == x && ORIGIN.1 == y { return (); }
                break;
            }
        }
    }

    pub fn count_sand_tiles(&self) -> usize {
        self.data.values().filter(|x| x == &&Tile::Sand).count()
    }
}

impl Display for Map {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut min = std::usize::MAX;
        let mut max = 0;
        for p in self.data.keys() {
            min = std::cmp::min(min, p.0);
            max = std::cmp::max(max, p.0);
        }
        min -= OFFSET;
        max += OFFSET + 1;

        for y in 0..self.height {
            write!(f, "|")?;
            for x in min..max {
                write!(f, "{}", self.get(x, y))?;
            }
            write!(f, "|\n")?;
        }
        Ok(())
    }
}
