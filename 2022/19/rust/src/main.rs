// -----------------------------------
// ---   This code doesn't work!   ---
// --- Saving for future debugging ---
// -----------------------------------
use rust::{ GEODE, Blueprint, State };
use std::fs::File;
use std::io::{ BufRead, BufReader };

const INPUT_FILE: &'static str = "../input/input.txt";

fn get_max(bp: &Blueprint, limit: u8) -> u16 {
    let mut max = 0;
    let mut queue: Vec<State> = Vec::new();
    queue.push(State::new(bp));

    while let Some(mut state) = queue.pop() {
        while state.time != limit && !state.should_build_something(limit) { state.mine(); }
        if state.time == limit - 1 { state.mine(); }

        if state.time == limit {
            max = std::cmp::max(max, state.materials[GEODE] as u16);
            continue;
        }

        if state.can_build(GEODE) {
            state.mine();
            state.build(GEODE);
            queue.push(state);
            continue;
        }

        for i in (0..3).rev() {
            if state.can_build(i) && state.should_build(i, limit) {
                let mut clone = state.clone();
                clone.mine();
                clone.build(i);
                queue.push(clone);
                state.mask |= 1 << i;
            }
        }

        state.mine();
        queue.push(state)
    }

    max
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = File::open(INPUT_FILE)?;
    let reader = BufReader::new(file);

    let bps: Vec<Blueprint> = reader.lines()
        .map(|line| line.unwrap().parse::<Blueprint>().unwrap())
        .collect();

    // println!("{:?}", get_max(&bps[0], 24));
    // for bp in bps { println!("{:?}\n{:?}", &bp, get_max(&bp, 24)); }

    let total: u16 = bps.iter().map(|bp| bp.id as u16 * get_max(bp, 24)).sum();
    println!("1. Total sum of quality levels: {}", total);

    let total: u16 = bps.iter().take(3).map(|bp| get_max(bp, 32)).fold(1, |acc, m| acc * m);
    println!("2. Total of multiply the first 3 blurprints max values: {}", total);

    Ok(())
}
