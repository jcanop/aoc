pub mod map;
pub use map::Map;

// --- Constants ---
pub const EMPTY: char = ' ';
pub const OPEN:  char = '.';
pub const WALL:  char = '#';
pub const RIGHT: char = 'R';
pub const LEFT:  char = 'L';
pub const EAST:  usize = 0;
pub const SOUTH: usize = 1;
pub const WEST:  usize = 2;
pub const NORTH: usize = 3;

// --- Types ---
pub type Grid = Vec<Vec<char>>;

// --- Move functions ---
pub fn move_grid(map: &mut Map, steps: usize) {
    let mut steps = steps;
    let mut x = map.x;
    let mut y = map.y;

    while steps > 0 {
        match map.d {
            NORTH => if y == 0 { y = map.height - 1; } else { y -= 1; },
            SOUTH => if y == map.height - 1 { y = 0; } else { y += 1; },
            WEST  => if x == 0 { x = map.width - 1;  } else { x -= 1; },
            EAST  => if x == map.width - 1  { x = 0; } else { x += 1; },
            _ => unreachable!()
        }
        if map.grid[y][x] == WALL { break; }
        if map.grid[y][x] == OPEN {
            steps -= 1;
            map.x = x;
            map.y = y;
        }
    }
}

pub fn move_cube(map: &mut Map, steps: usize) {
    let mut steps = steps;

    while steps > 0 {
        let mut x = map.x as isize;
        let mut y = map.y as isize;
        let mut d = map.d;

        match map.d {
            NORTH => y -= 1,
            SOUTH => y += 1,
            WEST  => x -= 1,
            EAST  => x += 1,
            _ => unreachable!()
        }

        if x < 0 || x >= map.width as isize || y < 0 || y >= map.height as isize || map.grid[y as usize][x as usize] == EMPTY {
            let mut r = map.y / 50;
            let mut c = map.x / 50;
            d = map.d;

            if      r == 0 && c == 1 && d == NORTH { r = 3; c = 0; d = EAST;  }
            else if r == 0 && c == 1 && d == WEST  { r = 2; c = 0; d = EAST;  }
            else if r == 0 && c == 2 && d == NORTH { r = 3; c = 0; d = NORTH; }
            else if r == 0 && c == 2 && d == EAST  { r = 2; c = 1; d = WEST;  }
            else if r == 0 && c == 2 && d == SOUTH { r = 1; c = 1; d = WEST;  }
            else if r == 1 && c == 1 && d == EAST  { r = 0; c = 2; d = NORTH; }
            else if r == 1 && c == 1 && d == WEST  { r = 2; c = 0; d = SOUTH; }
            else if r == 2 && c == 0 && d == NORTH { r = 1; c = 1; d = EAST;  }
            else if r == 2 && c == 0 && d == WEST  { r = 0; c = 1; d = EAST;  }
            else if r == 2 && c == 1 && d == EAST  { r = 0; c = 2; d = WEST;  }
            else if r == 2 && c == 1 && d == SOUTH { r = 3; c = 0; d = WEST;  }
            else if r == 3 && c == 0 && d == EAST  { r = 2; c = 1; d = NORTH; }
            else if r == 3 && c == 0 && d == SOUTH { r = 0; c = 2; d = SOUTH; }
            else if r == 3 && c == 0 && d == WEST  { r = 0; c = 1; d = SOUTH; }
            else { unreachable!(); }

            let ci = map.x % 50;
            let ri = map.y % 50;

            let i = match map.d {
                NORTH => ci,
                SOUTH => 49 - ci,
                WEST  => 49 - ri,
                EAST  => ri,
                _ => unreachable!()
            };

            let (rn , cn) = match d {
                NORTH => (49, i),
                SOUTH => (0, 49 - i),
                WEST  => (49 - i, 49),
                EAST  => (i, 0),
                _ => unreachable!()
            };

            x = (50 * c + cn) as isize;
            y = (50 * r + rn) as isize;
        }

        let x = x as usize;
        let y = y as usize;
        if map.grid[y][x] == WALL { break; }
        if map.grid[y][x] == OPEN {
            steps -= 1;
            map.x = x;
            map.y = y;
            map.d = d;
        }
    }
}
