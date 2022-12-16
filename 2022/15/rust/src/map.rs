use crate::sensor::Sensor;
use std::collections::HashMap;
use std::fmt::{ Display, Formatter };

pub type Point = (isize, isize);

pub struct Map {
    pub sensors: HashMap<Point, Sensor>,
    pub beacons: Vec<Point>
}

impl Map {
    pub fn new() -> Self {
        let sensors = HashMap::new();
        let beacons = Vec::new();
        Self { sensors, beacons }
    }

    pub fn add(&mut self, sx: isize, sy: isize, bx: isize, by: isize) {
        self.sensors.insert((sx, sy), Sensor::new(sx, sy, bx, by));
        self.beacons.push((bx, by));
    }

    pub fn is_in_sensor_range(&self, x: isize, y: isize) -> bool {
        for s in self.sensors.values() {
            if s.is_in_range(x, y) {
                return true;
            }
        }
        false
    }

    pub fn is_empty(&self, x: isize, y: isize) -> bool {
        let p = &(x, y);
        !self.sensors.contains_key(p) && !self.beacons.contains(p)
    }
}

impl Display for Map {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        for s in self.sensors.values() {
            write!(f, "{}\n", s)?;
        }
        Ok(())
    }
}
