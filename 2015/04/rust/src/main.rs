// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Parse input file ---
    let secret = std::fs::read_to_string(INPUT_FILE)?.trim().to_string();
    let mut found1 = false;
    let mut found2 = false;
    let mut count = 0;
    loop {
        // --- Compute the hash ---
        let text = format!("{}{}", &secret, count);
        let digest = md5::compute(text.as_bytes());
        let hex = format!("{:x}", digest);

        // --- Puzzle 1 ---
        if !found1 && &hex[0..5] == "00000" {
            println!("1. First hash with five zeros: {}", count);
            found1 = true;
        }

        // --- Puzzle 2 ---
        if !found2 && &hex[0..6] == "000000" {
            println!("2. First hash with six zeros: {}", count);
            found2 = true;
        }

        if found1 == true && found2 == true { break; }
        count += 1;
    }

    Ok(())
}
