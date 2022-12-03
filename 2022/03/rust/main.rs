use std::fs::File;
use std::io::{ BufRead, BufReader };

const INPUT_FILE:&'static str = "../input/input.txt";

fn find_duplicate(a: &str, b: &str, c: Option<&str>) -> char {
    for c1 in a.chars() {
        for c2 in b.chars() {
            if c1 == c2 {
                if let Some(cc) = c {
                    for c3 in cc.chars() {
                        if c2 == c3 {
                            return c3;
                        }
                    }
                } else {
                    return c1;
                }
            }
        }
    }
    panic!("Duplicated not found!");
}

fn get_priority(c: char) -> usize {
    if c >= 'a' && c <= 'z' {
        return c as usize - 'a' as usize + 1;
    }
    if c >= 'A' && c <= 'Z' {
        return c as usize - 'A' as usize + 27;
    }
    panic!("Invalid char: {}", c);
}

fn main() -> Result<(), std::io::Error> {
    // --- Puzzle: Part 1 ---
    let file = File::open(INPUT_FILE)?;
    let total: usize = BufReader::new(file).lines().map(|line| {
        let line = line.unwrap();
        let len = line.len() / 2;
        let c = find_duplicate(&line[0..len], &line[len..], Option::None);
        get_priority(c)
    }).sum();
    println!("Part 1. Total sum of the priorities: {}", total);

    // --- Puzzle: Part 2 ---
    let file = File::open(INPUT_FILE)?;
    let mut lines: Vec<String> = Vec::with_capacity(3);
    let mut index = 0;
    let mut total: usize = 0;
    for line in BufReader::new(file).lines() {
        lines.insert(index, line?);
        index += 1;

        if index == 3 {
            let a = &lines[0];
            let b = &lines[1];
            let c = Some(lines[2].as_str());
            let d = find_duplicate(a, b, c);
            total += get_priority(d);
            lines.clear();
            index = 0;
        }
    }
    println!("Part 2. Total sum of the item types: {}", total);

    Ok(())
}
