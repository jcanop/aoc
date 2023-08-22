use regex::Regex;

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = r"Enter the code at row (\d+), column (\d+)";
const FIRST_CODE: usize = 20151125;

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let regex = Regex::new(PARSER_REGEX)?;

    // --- Read and parse the input file ---
    let text = std::fs::read_to_string(INPUT_FILE)?;
    let cap = regex.captures(&text).unwrap();
    let row = cap[1].parse::<usize>()?;
    let column = cap[2].parse::<usize>()?;

    // --- Find the code ---
    let target = (((row + column - 1).pow(2) + row + column - 1) / 2) - ((row + column - 1) - column);
    let mut code = FIRST_CODE;
    for _ in 1 .. target {
        code = (code * 252533) % 33554393;
    }

    println!("Code: {}", code);
    Ok(())
}
