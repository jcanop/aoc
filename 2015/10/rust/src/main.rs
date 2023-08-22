// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";

fn look_and_say(s: String) -> String {
    let a: Vec<char> = s.chars().collect();
    let mut res: Vec<char> = Vec::new();
    let mut last = a[0];
    let mut count = 1;
    for i in 1 .. a.len() {
        if a[i] == last {
            count += 1;
        } else {
            res.extend(count.to_string().chars());
            res.push(last);
            count = 1;
        }
        last = a[i];
    }
    res.extend(count.to_string().chars());
    res.push(last);
    res.into_iter().collect()
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Read and parse the input file ---
    let mut text = std::fs::read_to_string(INPUT_FILE)?.trim().to_string();

    // --- Puzzle 1 ---
    for _ in 0 .. 40 {
        text = look_and_say(text);
    }
    println!("1. Length of the result: {}", text.len());

    // --- Puzzle 2 ---
    for _ in 0 .. 10 {
        text = look_and_say(text);
    }
    println!("2. Length of the result: {}", text.len());

    Ok(())
}
