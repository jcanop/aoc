use regex::Regex;
use serde_json::Value;

// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = r"(-?\d+)";

// --- Sum all number of a json, ignoring object with "red" attribute ---
fn sum(json: &Value) -> isize {
    match json {
        Value::Number(n) => n.as_i64().unwrap().try_into().unwrap(),
        Value::Array(a)  => a.iter().map(sum).sum(),
        Value::Object(o) => {
            for v in o.values() {
                if v.as_str() == Some("red") { return 0; }
            }
            o.values().map(sum).sum()
        },
        _ => 0
    }
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Read and parse the input file ---
    let text = std::fs::read_to_string(INPUT_FILE)?;

    // --- Puzzle 1 ---
    let regex = Regex::new(PARSER_REGEX)?;
    let total: isize = regex.captures_iter(&text)
        .map(|x| x[1].parse::<isize>().unwrap())
        .sum();
    println!("1. Sum of all numbers: {}", total);

    // --- Puzzle 2 ---
    let json: Value = serde_json::from_str(&text)?;
    let total = sum(&json);
    println!("2. Sum of all numbers: {}", total);

    Ok(())
}
