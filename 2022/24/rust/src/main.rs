use rust::Map;

// --- Constants ---
const INPUT_FILE: &'static str = "../input/input.txt";

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Read and parse the input file ---
    let mut map = Map::load(INPUT_FILE)?;
    let start = (1, 0);
    let end = (map.width - 2, map.height - 1);

    // --- Puzzle 1 ---
    let mut time = map.search(&start, &end, 0);
    println!("1. Minutes to reach the goal: {}", time);

    // --- Puzzle 2 ---
    time = map.search(&end, &start, time);
    time = map.search(&start, &end, time);
    println!("2. Minutes to reach the goal, go back to the start, then reach the goal again: {}", time);

    Ok(())
}
