const INPUT_FILE:&'static str = "../input/input.txt";

fn main() -> Result<(), std::io::Error> {
    let mut floor = 0;
    let mut pos = 0;
    let mut count = 0;

    // --- Read and parse the input file ---
    for c in std::fs::read_to_string(INPUT_FILE)?.chars() {
        count += 1;
        match c {
            ')' => floor -= 1,
            '(' => floor += 1,
            _   => ()
        }
        if pos == 0 && floor == -1 { pos = count; }
    }

    // --- Puzzle 1 ---
    println!("1. Floor: {}", floor);

    // --- Puzzle 2 ---
    println!("2. Position: {}", pos);
    Ok(())
}
