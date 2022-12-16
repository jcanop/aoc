use std::fmt::{ Display, Formatter };

pub struct Sensor {
    pub x: isize,
    pub y: isize,
    pub range: u64
}

impl Sensor {
    pub fn new(sx: isize, sy: isize, bx: isize, by: isize) -> Self {
        let range: u64 = (sx - bx).abs() as u64 + (sy - by).abs() as u64;
        Self { x: sx, y: sy, range }
    }

    pub fn is_in_range(&self, x: isize, y: isize) -> bool {
        (self.x - x).abs() as u64 + (self.y - y).abs() as u64 <= self.range
    }
}

impl Display for Sensor {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "[Sensor] {}, {}, range: {}", self.x, self.y, self.range)
    }
}
