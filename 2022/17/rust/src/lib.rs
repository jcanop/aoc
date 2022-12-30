// --- Constants ---
pub const SHAPES: [&Shape; 5] = [
    &[(0, 0), (1, 0), (2, 0), (3, 0)],
    &[(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
    &[(2, 0), (2, 1), (0, 2), (1, 2), (2, 2)],
    &[(0, 0), (0, 1), (0, 2), (0, 3)],
    &[(0, 0), (1, 0), (0, 1), (1, 1)]
];

const WIDTH: usize = 7;
const MARGIN_LEFT: usize = 2;
const MARGIN_TOP: usize = 3;
const AIR: char = '.';
const ROCK: char = '@';
const SAND: char = '#';
const LEFT: char = '<';
const RIGHT: char = '>';
const DOWN: char = 'V';

type Shape = [(usize, usize)];

pub struct Chamber {
    rows: Vec<[char; WIDTH]>,
    shape_len: usize
}

impl Chamber {
    pub fn new() -> Self {
        let rows = Vec::new();
        let shape_len = 0;
        Self { rows, shape_len }
    }

    pub fn height(&self) -> Option<usize> {
        match self.find_from_top(SAND) {
            Some(h) => Some(h - 1),
            None => None
        }
    }

    fn find_from_top(&self, c: char) -> Option<usize> {
        match self.rows.iter().enumerate().rev().find(|(_, x)| x.contains(&c)) {
            Some((i, _)) => Some(i),
            None => None
        }
    }

    pub fn add_shape(&mut self, shape: &Shape) {
        // --- Remove extra rows ---
        if let Some(top) = self.find_from_top(SAND) {
            let n = self.rows.len() - top - 1;
            for _ in 0..n { self.rows.remove(self.rows.len() - 1); }
        }

        // --- Add rows if needed ---
        self.shape_len = shape[shape.len() - 1].1 + 1;
        let n = MARGIN_TOP + self.shape_len;
        for _ in 0..n {
            let row: [char; WIDTH] = [AIR; WIDTH];
            self.rows.push(row);
        }

        // --- Add the shape ---
        let height = self.rows.len() - 1;
        for (x, y) in shape {
            self.rows[height - y][MARGIN_LEFT + x] = ROCK;
        }
    }

    fn can_move(&self, direction: char) -> bool {
        let mut i = self.find_from_top(ROCK).unwrap();
        if direction == DOWN {
            i -= self.shape_len - 1;
            if i == 0 { return false; }
            for y in i..i + self.shape_len {
                let up = self.rows[y];
                let down = self.rows[y - 1];
                for x in 0..WIDTH {
                    if up[x] == ROCK && down[x] == SAND { return false; }
                }
            }
        } else if direction == LEFT {
            for y in (i + 1 - self.shape_len ..= i).rev() {
                let row = self.rows[y];
                if row[0] == ROCK { return false; }
                for x in 1..WIDTH {
                    if row[x] == ROCK {
                        if row[x - 1] != AIR { return false; }
                        break;
                    }
                }
            }
        } else if direction == RIGHT {
            for y in (i + 1 - self.shape_len ..= i).rev() {
                let row = self.rows[y];
                if row[WIDTH - 1] == ROCK { return false; }
                for x in (0 ..= WIDTH - 2).rev() {
                    if row[x] == ROCK {
                        if row[x + 1] != AIR { return false; }
                        break;
                    }
                }
            }
        } else {
            panic!("Usupported direction: {}", direction);
        }
        true
    }

    pub fn move_down(&mut self) -> bool {
        self.move_shape(DOWN)
    }

    pub fn move_shape(&mut self, direction: char) -> bool {
        if self.can_move(direction) {
            if direction == DOWN {
                let row = self.find_from_top(ROCK).unwrap() - (self.shape_len - 1);
                for y in row..row + self.shape_len {
                    for x in 0..WIDTH {
                        if self.rows[y][x] == ROCK {
                            self.rows[y - 1][x] = self.rows[y][x];
                            self.rows[y][x] = AIR;
                        }
                    }
                }
            } else if direction == LEFT {
                let i = self.find_from_top(ROCK).unwrap();
                for y in (i + 1 - self.shape_len ..= i).rev() {
                    for x in 0..WIDTH - 1 {
                        if self.rows[y][x + 1] == ROCK {
                            self.rows[y][x] = self.rows[y][x + 1];
                            self.rows[y][x + 1] = AIR;
                        }
                    }
                }
            } else if direction == RIGHT {
                let i = self.find_from_top(ROCK).unwrap();
                for y in (i + 1 - self.shape_len ..= i).rev() {
                    for x in (1..WIDTH).rev() {
                        if self.rows[y][x - 1] == ROCK {
                            self.rows[y][x] = self.rows[y][x - 1];
                            self.rows[y][x - 1] = AIR;
                        }
                    }
                }
            } else {
                panic!("Usupported direction: {}", direction);
            }
        } else if direction == DOWN {
            let i = self.find_from_top(ROCK).unwrap() - (self.shape_len - 1);
            for y in i..i + self.shape_len {
                for x in 0..WIDTH {
                    if self.rows[y][x] == ROCK { self.rows[y][x] = SAND; }
                }
            }
            return false;
        }
        true
    }

    pub fn print(&self) {
        for row in self.rows.iter().rev() {
            print!("|");
            for c in row { print!("{}", c); }
            println!("|");
        }
        print!("+");
        for _ in 0..WIDTH { print!("-"); }
        println!("+");
    }
}
