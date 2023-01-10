use rust::{ GEODE, Blueprint, State };
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants ---
const INPUT_FILE: &'static str = "../input/input.txt";

// --- Find the optimal path for a blueprint ---
fn get_max(bp: &Blueprint, limit: u8) -> u16 {
    let mut max = 0;
    let mut queue: Vec<State> = Vec::new();
    queue.push(State::new(bp));

    while let Some(mut state) = queue.pop() {
        if state.time == limit - 1 { state.mine(); }

        if state.time == limit {
            max = std::cmp::max(max, state.materials[GEODE] as u16);
            continue;
        }

        if state.should_build(GEODE) {
            state.mine();
            state.build(GEODE);
            queue.push(state);
            continue;
        }

        for i in (0..3).rev() {
            if state.should_build(i) {
                let mut clone = state.clone();
                clone.mine();
                clone.build(i);
                clone.mask = 0;
                queue.push(clone);
                state.mask |= 1 << i;
            }
        }

        state.mine();
        queue.push(state)
    }

    max
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    let reader = BufReader::new(file);
    let bps: Vec<Blueprint> = reader.lines()
        .map(|line| line.unwrap().parse::<Blueprint>().unwrap())
        .collect();

    // --- Puzzle 1 ---
    let total: u16 = bps.iter().map(|bp| bp.id as u16 * get_max(bp, 24)).sum();
    println!("1. Total sum of quality levels: {}", total);

    // --- Puzzle 2 ---
    let total: u16 = bps.iter().take(3).map(|bp| get_max(bp, 32)).fold(1, |acc, m| acc * m);
    println!("2. Total of multiply the first 3 blurprints max values: {}", total);

    Ok(())
}
