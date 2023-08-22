use std::collections::HashSet;

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";

// --- Structures ---
struct Point {
    x: isize,
    y: isize
}

impl Default for Point {
    fn default() -> Self {
        Point { x: 0, y: 0 }
    }
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut p: Point = Point::default();
    let mut s: Point = Point::default();
    let mut r: Point = Point::default();

    let mut set1: HashSet<String> = HashSet::new();
    let mut set2: HashSet<String> = HashSet::new();
    set1.insert("0,0".to_string());
    set2.insert("0,0".to_string());

    let mut n = 0;

    // --- Parse the input file ---
    let text = std::fs::read_to_string(INPUT_FILE)?;
    for c in text.chars() {
        n += 1;
        match c {
            '^' => { p.y -= 1; if n % 2 == 0 { r.y -= 1 } else { s.y -= 1 } },
            'v' => { p.y += 1; if n % 2 == 0 { r.y += 1 } else { s.y += 1 } },
            '<' => { p.x -= 1; if n % 2 == 0 { r.x -= 1 } else { s.x -= 1 } },
            '>' => { p.x += 1; if n % 2 == 0 { r.x += 1 } else { s.x += 1 } },
            _   => ()
        }

        let point = format!("{},{}", p.x, p.y);
        set1.insert(point);

        let point = if n % 2 == 0 {
            format!("{},{}", r.x, r.y)
        } else {
            format!("{},{}", s.x, s.y)
        };
        set2.insert(point);
    }

    // --- Puzzle 1 ---
    println!("1. Houses with at least one present: {}", set1.len());

    // --- Puzzle 2 ---
    println!("2. Houses with at least one present: {}", set2.len());

    Ok(())
}
