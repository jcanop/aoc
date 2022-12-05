use regex::Regex;
use std::fs::File;
use std::io::{ BufRead, BufReader };

const INPUT_FILE:&'static str = "../input/input.txt";

fn main() -> Result<(), std::io::Error> {
    let crates_regex: Regex = Regex::new(r"(\[\w\]|\s{3})\s?").unwrap();
    let steps_regex: Regex = Regex::new(r"\d+").unwrap();

    let mut queues1: Vec<Vec<char>> = Vec::new();
    let mut queues2: Vec<Vec<char>> = Vec::new();
    let mut setup: bool = true;

    // --- Parse input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();

        // --- Parse Initial crates setup ---
        if setup {
            if line.contains('[') {
                for (i, token) in crates_regex.find_iter(&line).enumerate() {
                    let c = token.as_str().chars().nth(1).unwrap();
                    if queues1.len() < i + 1 { queues1.push(Vec::new()); }
                    if queues2.len() < i + 1 { queues2.push(Vec::new()); }
                    if c != ' ' {
                        queues1[i].push(c);
                        queues2[i].push(c);
                    }
                }

            }

            // --- An empty line marks ends of setup ---
            if line.len() == 0 { setup = false; }
            continue;
        }

        // --- Parse script ---
        let tokens: Vec<usize> = steps_regex.find_iter(&line)
            .map(|x| x.as_str().parse::<usize>().unwrap())
            .collect();
        let count = tokens[0];
        let from = tokens[1] - 1;
        let to = tokens[2] - 1;

        // --- Puzzle 1 ---
        for _ in 0..count {
            let c = queues1[from].remove(0);
            queues1[to].insert(0, c);
        }

        // --- Puzzle 2 ---
        for i in (0..count).rev() {
            let c = queues2[from].remove(i);
            queues2[to].insert(0, c);
        }
    }

    print!("Part 1. Crates on the top of each stack: ");
    for queue in queues1 { print!("{}", &queue[0]) }
    println!();

    print!("Part 2. Crates on the top of each stack: ");
    for queue in queues2 { print!("{}", &queue[0]) }
    println!();

    Ok(())
}
