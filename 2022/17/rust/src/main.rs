use rust::{ SHAPES, Chamber };

// --- Constants ---
const INPUT_FILE: &'static str = "../input/input.txt";
const LIMIT_1: u64 = 2022;
const LIMIT_2: u64 = 1_000_000_000_000;

// --- Runs the simulations ---
fn sim(flows: &[char], limit: u64) -> u64 {
    let mut count = 0;
    let mut rocks = 1;
    let gcd = SHAPES.len() * flows.len(); // Greates Common Divisor
    let mut chamber = Chamber::new();
    chamber.add_shape(SHAPES[count % SHAPES.len()]);

    // --- Variables for tracking a pattern ---
    let mut prev_height: u64 = 0;
    let mut prev_rocks: u64 = 0;
    let mut delta_height: u64  = 0;
    let mut delta_rocks: u64 = 0;
    let mut simulated: u64 = 0;

    // --- Iterate until the limit is reached ---
    loop {
        // --- Tries to find a pattern ---
        if count > 0 && count % gcd == 0 {
            let height = chamber.height().unwrap() as u64;
            let dh = height - prev_height;
            let dr = rocks - prev_rocks;
            // --- Pattern found ---
            if dh == delta_height && dr == delta_rocks {
                let rate = (limit - rocks) / dr;
                simulated = dh * rate + 1;
                rocks = limit - ((limit - rocks) % dr);
            }
            prev_height = height;
            prev_rocks = rocks;
            delta_height = dh;
            delta_rocks = dr;
        }

        // --- Normal simulation ---
        chamber.move_shape(flows[count % flows.len()]);
        if !chamber.move_down() {
            if rocks > limit {
                return chamber.height().unwrap() as u64 + simulated;
            }
            chamber.add_shape(SHAPES[rocks as usize % SHAPES.len()]);
            rocks += 1;
        }
        count += 1;
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Read and parse the input file ---
    let text = std::fs::read_to_string(INPUT_FILE)?;
    let text = text.trim();
    let flows: Vec<char> = text.chars().collect();

    // --- Puzzle 1 ---
    let height = sim(&flows, LIMIT_1);
    println!("1. The tower is {} units tall", height);

    // --- Puzzle 2 ---
    let height = sim(&flows, LIMIT_2);
    println!("2. The tower is {} units tall", height);

    Ok(())
}
