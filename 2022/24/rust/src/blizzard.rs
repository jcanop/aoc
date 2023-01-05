// --- Constants ---
const NORTH: char = '^';
const SOUTH: char = 'v';
const WEST:  char = '<';
const EAST:  char = '>';
const NONE:  char = '.';

// --- Blizzards going east and west ---
struct Row {
    east: Vec<bool>,
    west: Vec<bool>
}

impl Row {
    fn new() -> Self {
        let east = Vec::new();
        let west = Vec::new();
        Self { east, west }
    }

    fn add(&mut self, dir: char) {
        match dir {
            EAST => { self.east.push(true);  self.west.push(false); },
            WEST => { self.east.push(false); self.west.push(true);  },
            NORTH | SOUTH | NONE => { self.east.push(false); self.west.push(false); },
            _ => unreachable!()
        }
    }

    fn is_empty(&self, x: usize, time: usize) -> bool {
        let len = self.east.len();
        let wi = (x - 1 + time) % len;
        let ei = (x - 1 + len - (time % len)) % len;
        !self.east[ei] && !self.west[wi]
    }
}

// --- Blizzards going north and south ---
struct Col {
    north: Vec<bool>,
    south: Vec<bool>
}

impl Col {
    fn new() -> Self {
        let north = Vec::new();
        let south = Vec::new();
        Self { north, south }
    }

    fn add(&mut self, dir: char) {
        match dir {
            NORTH => { self.north.push(true);  self.south.push(false); },
            SOUTH => { self.north.push(false); self.south.push(true);  },
            WEST | EAST | NONE  => { self.north.push(false); self.south.push(false); },
            _ => unreachable!()
        }
    }

    fn is_empty(&self, y: usize, time: usize) -> bool {
        let len = self.north.len();
        let ni = (y - 1 + time) % len;
        let si = (y - 1 + len - (time % len)) % len;
        !self.north[ni] && !self.south[si]
    }
}

// --- Blizzard struct ---
pub struct Blizzard {
    cols: Vec<Col>,
    rows: Vec<Row>
}

impl Blizzard {
    pub fn new() -> Self {
        let cols = Vec::new();
        let rows = Vec::new();
        Self { cols, rows }
    }

    pub fn add(&mut self, line: &str) {
        let mut row = Row::new();
        while self.cols.len() < line.len() { self.cols.push(Col::new()); }
        for (x, d) in line.chars().enumerate() {
            self.cols.get_mut(x).unwrap().add(d);
            row.add(d);
        }
        self.rows.push(row);
    }

    pub fn is_empty(&self, x: usize, y: usize, time: usize) -> bool {
        self.cols[x - 1].is_empty(y, time) && self.rows[y - 1].is_empty(x, time)
    }
}
