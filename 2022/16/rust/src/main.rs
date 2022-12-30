use rust::Layout;

const INPUT_FILE: &'static str = "../input/input.txt";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let layout = Layout::load(INPUT_FILE)?;

    // --- Puzzle 1 ---
    let total = layout.dfs1();
    println!("1. Total pressure released: {}", total);

    // --- Puzzle 2 ---
    let total = layout.dfs2();
    println!("2. Total pressure released with an elephant: {}", total);

    Ok(())
}
