use rust::Grove;

// --- Constants ---
const INPUT_FILE: &'static str = "../input/input.txt";

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut grove = Grove::load(INPUT_FILE)?;
    let mut round = 1;
    while grove.sim() {
        if round == 10 {
            let total = grove.empty_count();
            println!("1. Empty ground tiles: {}", total);
        }
        round += 1;
    }
    println!("2. First round with no Elf moves: {}", round);
    Ok(())
}
