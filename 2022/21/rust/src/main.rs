use rust::Resolver;

const INPUT_FILE: &'static str = "../input/input.txt";
const ROOT: &'static str = "root";
const ME: &'static str = "humn";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Puzzle 1 ---
    let resolver = Resolver::load(INPUT_FILE)?;
    let result = resolver.solve_for(ROOT);
    println!("1. Number that will the monkey root yell: {}", result);

    // --- Puzzle 2 ---
    let mut resolver = Resolver::load(INPUT_FILE)?;
    resolver.update_for_puzzle2(ROOT, ME);
    let result = resolver.solve_for(ME);
    println!("2. Number that I yell to pass root's equality test: {}", result);

    Ok(())
}
