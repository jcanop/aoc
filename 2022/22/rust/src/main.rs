use rust::Map;

// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Reads and Parse the input file ---
    let text = std::fs::read_to_string(INPUT_FILE)?;
    let i = text.find("\n\n").unwrap();
    let grid = &text[0..i];
    let code = &text[i + 2..];

    // --- Puzzle 1 --
    let mut map = Map::load(grid);
    map.path(code, rust::move_grid);
    let password = map.get_password();
    println!("1. Password {}", password);

    // --- Puzzle 2 ---
    let mut map = Map::load(grid);
    map.path(code, rust::move_cube);
    let password = map.get_password();
    println!("2. Password {}", password);

    Ok(())
}
