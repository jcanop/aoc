pub mod map;
pub mod sensor;

use map::Map;
use regex::Regex;

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = "Sensor at x=(?:(-?\\d+)), y=(?:(-?\\d+)): \
    closest beacon is at x=(?:(-?\\d+)), y=(?:(-?\\d+))";
const PUZZLE1_LINE: isize = 2_000_000;
const PUZZLE2_MIN: isize = 0;
const PUZZLE2_MAX: isize = 4_000_000;

// --- Helper functions ---
fn in_limit(x: isize, y: isize) -> bool {
    x >= PUZZLE2_MIN && x <= PUZZLE2_MAX && y >= PUZZLE2_MIN && y <= PUZZLE2_MAX
}

fn calculate_freq(x: isize, y: isize) -> u64 {
    let x = x as u64;
    let y = y as u64;
    let m = PUZZLE2_MAX as u64;
    x * m + y
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut map = Map::new();

    // --- Read and parse the input file ---
    let text = std::fs::read_to_string(INPUT_FILE)?;
    let regex = Regex::new(PARSER_REGEX)?;
    for cap in regex.captures_iter(&text) {
        let sx: isize = cap[1].parse()?;
        let sy: isize = cap[2].parse()?;
        let bx: isize = cap[3].parse()?;
        let by: isize = cap[4].parse()?;
        map.add(sx, sy, bx, by);
    }

    // --- Search Max and Min ---
    let mut min = isize::max_value();
    let mut max = isize::min_value();
    let mut range = 0;
    for s in map.sensors.values() {
        min = std::cmp::min(min, s.x);
        max = std::cmp::max(max, s.x);
        range = std::cmp::max(range, s.range);
    }
    min -= range as isize;
    max += range as isize;

    // --- Puzzle 1 --
    let mut total: u64 = 0;
    for x in min..=max {
        if map.is_in_sensor_range(x, PUZZLE1_LINE) && map.is_empty(x, PUZZLE1_LINE) {
            total += 1;
        }
    }
    println!("1. Positions that cannot contain a beacon: {}", total);

    // --- Puzzle 2 ---
    let mut freq = 0;
    for s in map.sensors.values() {
        let r = s.range as isize;
        for i in 0..=r {
            // North -> East
            let x = s.x + i;
            let y = s.y - r + i - 1;
            if in_limit(x, y) && !map.is_in_sensor_range(x, y) {
                freq = calculate_freq(x, y); break;
            }

            // East -> South
            let x = s.x + r - i + 1;
            let y = s.y + i;
            if in_limit(x, y) && !map.is_in_sensor_range(x, y) {
                freq = calculate_freq(x, y); break;
            }

            // South -> West
            let x = s.x - i;
            let y = s.y + r - i + 1;
            if in_limit(x, y) && !map.is_in_sensor_range(x, y) {
                freq = calculate_freq(x, y); break;
            }

            // West -> North
            let x = s.x - r + i - 1;
            let y = s.y - i;
            if in_limit(x, y) && !map.is_in_sensor_range(x, y) {
                freq = calculate_freq(x, y); break;
            }
        }
        if freq > 0 { break; }
    }
    println!("2. Frequency: {}", freq);

    Ok(())
}
