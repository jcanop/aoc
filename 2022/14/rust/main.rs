pub mod map;

use map::Map;

// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Parse and read the input file ---
    let mut map: Map = Map::load(INPUT_FILE)?;

    // --- Puzzle 1 ---
    map.simulate(1);
    let total = map.count_sand_tiles();
    println!("1. Total units of sand: {}", total);

    // --- Puzzle 2 ---
    map.simulate(2);
    let total = map.count_sand_tiles();
    println!("2. Total units of sand: {}", total);

    Ok(())
}
