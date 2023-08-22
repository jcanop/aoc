use std::collections::HashMap;
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";

fn solve(op: &str, wires: &HashMap<String, String>, cache: &mut HashMap<String, u16>) -> u16 {
    if let Ok(n) = op.trim().parse::<u16>() {
        return n;
    }
    if let Some(n) = cache.get(op) {
        return *n;
    }
    let ls:Vec<&str> = wires.get(op).unwrap().split(" ").collect();
    let r = if ls.contains(&"AND") {
        solve(ls[0], wires, cache) & solve(ls[2], wires, cache)
    } else if ls.contains(&"OR") {
        solve(ls[0], wires, cache) | solve(ls[2], wires, cache)
    } else if ls.contains(&"LSHIFT") {
        solve(ls[0], wires, cache) << solve(ls[2], wires, cache)
    } else if ls.contains(&"RSHIFT") {
        solve(ls[0], wires, cache) >> solve(ls[2], wires, cache)
    } else if ls.contains(&"NOT") {
        !solve(ls[1], wires, cache)
    } else {
        solve(ls[0], wires, cache)
    };
    cache.insert(op.to_string(), r);
    return r;
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut wires: HashMap<String, String> = HashMap::new();
    let mut cache: HashMap<String, u16> = HashMap::new();

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        let v: Vec<&str> = line.split(" -> ").collect();
        wires.insert(v[1].trim().to_string(), v[0].trim().to_string());
    }

    // --- Puzzle 1 ---
    let r = solve("a", &wires, &mut cache);
    println!("1. Wire a: {}", r);

    // --- Puzzle 2 ---
    wires.insert("b".to_string(), format!("{}", r));
    cache.clear();
    let r = solve("a", &wires, &mut cache);
    println!("2. Wire a: {}", r);

    Ok(())
}
