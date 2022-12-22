use regex::Regex;
use std::str::FromStr;

// --- Constants ---
const ORE: usize = 0;
const CLAY: usize = 1;
const OBSIDIAN: usize = 2;
pub const GEODE: usize = 3;

const REGEX: &'static str = "Blueprint (?:(\\d+)): \
    Each ore robot costs (?:(\\d+)) ore. \
    Each clay robot costs (?:(\\d+)) ore. \
    Each obsidian robot costs (?:(\\d+)) ore and (?:(\\d+)) clay. \
    Each geode robot costs (?:(\\d+)) ore and (?:(\\d+)) obsidian.";

#[derive(Debug)]
pub struct Blueprint {
    pub id: u8,
    costs: [[u8; 3]; 4],
    max_costs: [u8; 3]
}

impl FromStr for Blueprint {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let regex = Regex::new(REGEX).unwrap();
        let cap = regex.captures(s).unwrap();
        let id = cap[1].parse()?;
        let mut costs = [[0; 3]; 4];
        costs[ORE][ORE] = cap[2].parse()?;
        costs[CLAY][ORE] = cap[3].parse()?;
        costs[OBSIDIAN][ORE] = cap[4].parse()?;
        costs[OBSIDIAN][CLAY] = cap[5].parse()?;
        costs[GEODE][ORE] = cap[6].parse()?;
        costs[GEODE][OBSIDIAN] = cap[7].parse()?;
        let mut max_costs = [0; 3];
        for i in 0..3 {
            for j in 0..4 {
                max_costs[i] = std::cmp::max(max_costs[i], costs[j][i]);
            }
        }
        Ok(Blueprint { id, costs, max_costs })
    }
}

#[derive(Clone, Debug)]
pub struct State<'a> {
    bp: &'a Blueprint,
    robots: [u8; 4],
    pub materials: [u8; 4],
    pub mask: u8,
    pub time: u8
}

impl<'a> State<'a> {
    pub fn new(bp: &'a Blueprint) -> Self {
        let mut robots = [0;  4];
        let materials = [0; 4];
        let mask = 0;
        let time = 0;
        robots[ORE] = 1;
        Self { bp, robots, materials, mask, time }
    }

    pub fn mine(&mut self) {
        for i in 0..4 {
            self.materials[i] += self.robots[i];
        }
        self.time += 1;
    }

    pub fn can_build(&self, robot: usize) -> bool {
        for i in 0..3 {
            if self.materials[i] < self.bp.costs[robot][i] { return false; }
        }
        true
    }

    pub fn should_build(&self, robot: usize, limit: u8) -> bool {
        if robot == GEODE { return true; }
        let m = 1 << robot;
        if (self.mask & m) == m { return false; }
        let max = self.bp.max_costs[robot];
        if self.materials[robot] as u16 >= (limit - self.time) as u16 * max as u16 { return false; }
        match robot {
            ORE => self.robots[ORE] < max,
            CLAY => self.robots[CLAY] < self.bp.costs[OBSIDIAN][CLAY],
            OBSIDIAN => self.robots[OBSIDIAN] < self.bp.costs[GEODE][OBSIDIAN],
            _ => panic!("Insupported robot: {}", robot)
        }
    }

    pub fn should_build_something(&self, limit: u8) -> bool {
        for i in (0..4).rev() {
            if self.can_build(i) && self.should_build(i, limit) { return true; }
        }
        false
    }

    pub fn build(&mut self, robot: usize) {
        for i in 0..3 {
            self.materials[i] -= self.bp.costs[robot][i];
        }
        self.robots[robot] += 1;
    }
}
