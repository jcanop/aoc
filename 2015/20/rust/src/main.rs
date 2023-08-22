// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Read and parse the input file ---
    let input = std::fs::read_to_string(INPUT_FILE)?.trim().parse::<usize>()?;

    // --- Puzzle 1 ---
    let mut house = 0;
    let mut presents = 0;
    while presents < input {
        house += 1;
        presents = 10 * divisors::get_divisors(house).iter().sum::<usize>();
        // --- divisors doen't take into account 1 as divisor or the number itself ---
        if house <= 2 {
            presents += 10;
        } else {
            presents += 10 * (house + 1);
        }
    }
    println!("1. House number: {}", house);

    // --- Puzzle 2 ---
    let mut house = 0;
    let mut presents = 0;
    while presents < input {
        house += 1;
        presents = 11 * divisors::get_divisors(house).iter()
            .filter(|x| 50 * **x >= house)
            .sum::<usize>();
        // --- divisors doen't take into account 1 as divisor or the number itself ---
        if house <= 2 {
            presents += 11;
        } else {
            if house <= 50 { presents += 11; }
            presents += 11 * house;
        }
    }
    println!("2. House number: {}", house);

    Ok(())
}
