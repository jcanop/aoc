use std::collections::HashSet;
use std::fs::File;
use std::io::{ BufRead, BufReader };
use std::str::FromStr;

// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";

// Represents a direction.
enum Direction {
    Up, Down, Left, Right
}

impl FromStr for Direction {
    type Err = std::string::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "U" => return Ok(Direction::Up),
            "D" => return Ok(Direction::Down),
            "L" => return Ok(Direction::Left),
            "R" => return Ok(Direction::Right),
            _ => panic!("Unknown direction: {}", s)
        }
    }
}

// Represents a Knot in a rope.
struct Knot {
    x: isize,
    y: isize
}

impl Knot {
    fn new () -> Self {
        Knot { x: 0, y: 0 }
    }
}

// Represents a Rope.
struct Rope {
    list: Vec<Knot>,
    trail: HashSet<(isize, isize)>
}

impl Rope {
    fn new(len: usize) -> Self {
        let mut list = Vec::with_capacity(len);
        for _ in 0..len {
            list.push(Knot::new());
        }
        let mut trail = HashSet::new();
        trail.insert((0, 0));
        Rope { list, trail }
    }

    fn r#move(&mut self, direction: &Direction) {
        match direction {
            Direction::Up    => self.list[0].y -= 1,
            Direction::Down  => self.list[0].y += 1,
            Direction::Left  => self.list[0].x -= 1,
            Direction::Right => self.list[0].x += 1
        }

        for i in 1..self.list.len() {
            let c = i - 1;
            let n = i;
            let dx = self.list[c].x - self.list[n].x;
            let dy = self.list[c].y - self.list[n].y;
            if dx.abs() < 2 && dy.abs() < 2 { return (); }
            self.list[n].x += dx.signum();
            self.list[n].y += dy.signum();

            if i == self.list.len() - 1 {
                self.trail.insert((self.list[n].x, self.list[n].y));
            }
        }
    }
}

fn main() -> std::io::Result<()> {
    let mut rope1 = Rope::new(2);
    let mut rope2 = Rope::new(10);

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        let dir: Direction = line[0..1].parse().unwrap();
        let count: usize = line[2..].parse().unwrap();
        for _ in 0..count {
            rope1.r#move(&dir);
            rope2.r#move(&dir);
        }
    }

    println!("1. Position visited at least once: {}", rope1.trail.len());
    println!("2. Position visited at least once: {}", rope2.trail.len());

    Ok(())
}
