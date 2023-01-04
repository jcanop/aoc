use crate::Point;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants ---
const OFFSET: usize = 10_000;

pub struct Grove {
    elves: HashSet<Point>,
    proposed: HashMap<Point, Point>,
    banned: HashSet<Point>,
    round: usize
}

impl Grove {
    // --- Creates a Grove from an input file ---
    pub fn load(filename: &str) -> std::io::Result<Self> {
        let mut elves = HashSet::new();
        let proposed = HashMap::new();
        let banned = HashSet::new();
        let round = 0;

        let file = File::open(filename)?;
        for (y, line) in BufReader::new(file).lines().enumerate() {
            for (x, c) in line.unwrap().chars().enumerate() {
                if c == '#' {
                    let x = (OFFSET + x) as isize;
                    let y = (OFFSET + y) as isize;
                    elves.insert((x, y));
                }
            }
        }

        Ok(Grove{ elves, proposed, banned, round })
    }

    // --- Simulates one full round ---
    pub fn sim(&mut self) -> bool {
        let list: Vec<&Point> = self.elves.iter().collect();
        let mut result = false;

        // --- Phase 1: Considers move ---
        for pos in list {
            let mut found = false;
            for y in -1..=1 {
                for x in -1..=1 {
                    if x == 0 && y == 0 { continue; }
                    let p = (pos.0 + x, pos.1 + y);
                    if self.elves.contains(&p) {
                        found = true;
                        break;
                    }
                }
                if found { break; }
            }
            if !found {
                // --- No Elf adjacent, then do nothing ---
                self.proposed.insert(*pos, *pos);
                continue;
            }

            // --- Look at the 4 directions ---
            let mut p: Option<Point> = None;
            for n in 0..4 {
                let i = (n + self.round) % 4;
                if i == 0 { // North
                    if  self.elves.contains(&(pos.0 - 1, pos.1 - 1)) ||
                        self.elves.contains(&(pos.0    , pos.1 - 1)) ||
                        self.elves.contains(&(pos.0 + 1, pos.1 - 1)) { continue; }
                    p = Some((pos.0, pos.1 - 1));
                    break;
                } else if i == 1 { // South
                    if  self.elves.contains(&(pos.0 - 1, pos.1 + 1)) ||
                        self.elves.contains(&(pos.0    , pos.1 + 1)) ||
                        self.elves.contains(&(pos.0 + 1, pos.1 + 1)) { continue; }
                    p = Some((pos.0, pos.1 + 1));
                    break;
                } else if i == 2 { // West
                    if  self.elves.contains(&(pos.0 - 1, pos.1 - 1)) ||
                        self.elves.contains(&(pos.0 - 1, pos.1    )) ||
                        self.elves.contains(&(pos.0 - 1, pos.1 + 1)) { continue; }
                    p = Some((pos.0 - 1, pos.1));
                    break;
                } else if i == 3 { // East
                    if  self.elves.contains(&(pos.0 + 1, pos.1 - 1)) ||
                        self.elves.contains(&(pos.0 + 1, pos.1    )) ||
                        self.elves.contains(&(pos.0 + 1, pos.1 + 1)) { continue; }
                    p = Some((pos.0 + 1, pos.1));
                    break;
                } else {
                    unreachable!();
                }
            }

            if let Some(p) = p {
                if self.banned.contains(&p) {
                    self.proposed.insert(*pos, *pos);
                } else if self.proposed.contains_key(&p) {
                    self.banned.insert(p);
                    let e = self.proposed.remove(&p).unwrap();
                    self.proposed.insert(e, e);
                    self.proposed.insert(*pos, *pos);
                } else {
                    self.proposed.insert(p, *pos);
                }
                result = true;
            } else {
                self.proposed.insert(*pos, *pos);
            }
        }

        // --- Phase 2: Move ---
        self.elves.clear();
        for pos in self.proposed.keys() {
            self.elves.insert(*pos);
        }
        self.proposed.clear();
        self.banned.clear();
        self.round += 1;

        result
    }

    // --- Calculates the view rectangle ---
    fn find_view_rect(&self) -> (usize, usize, usize, usize) {
        let mut min_x = usize::max_value();
        let mut min_y = usize::max_value();
        let mut max_x = 0;
        let mut max_y = 0;
        for p in &self.elves {
            min_x = usize::min(min_x, p.0 as usize);
            min_y = usize::min(min_y, p.1 as usize);
            max_x = usize::max(max_x, p.0 as usize);
            max_y = usize::max(max_y, p.1 as usize);
        }
        (min_x, min_y, max_x, max_y)
    }

    // --- Count the empty titles ---
    pub fn empty_count(&self) -> usize {
        let mut total = 0;
        let (min_x, min_y, max_x, max_y) = self.find_view_rect();
        for y in min_y ..= max_y {
            for x in min_x ..= max_x {
                if !self.elves.contains(&(x as isize, y as isize)) { total += 1 };
            }
        }
        total
    }
}
