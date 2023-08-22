use regex::Regex;
use std::fs::File;
use std::io::{ BufRead, BufReader };

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = r"^[a-zA-Z]+: capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)$";
const MAX:usize = 100;

// --- Structs ---
#[derive(Debug)]
struct Ingredient {
    capacity: isize,
    durability: isize,
    flavor: isize,
    texture: isize,
    calories: isize
}

// --- Returns zoro when the number is negative ---
fn not_negative(n: isize) -> usize {
    if n < 0 { return 0; }
    n as usize
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let regex = Regex::new(PARSER_REGEX)?;
    let mut ingredients: Vec<Ingredient> = Vec::new();
    let mut max1: usize = std::usize::MIN;
    let mut max2: usize = std::usize::MIN;

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        for cap in regex.captures_iter(&line) {
            let capacity = cap[1].parse()?;
            let durability = cap[2].parse()?;
            let flavor = cap[3].parse()?;
            let texture = cap[4].parse()?;
            let calories = cap[5].parse()?;
            let ingredient = Ingredient { capacity, durability, flavor, texture, calories };
            ingredients.push(ingredient);
        }
    }

    // --- Calculate combinations ---
    let mut list: Vec<usize> = Vec::new();
    for _ in 0 .. ingredients.len() - 1 { list.push(0); }
    list.push(MAX);

    let last = list.len() - 1;
    'outer: while list[0] <= MAX {
        // --- Create cookie ---
        let mut capacity = 0;
        let mut durability = 0;
        let mut flavor = 0;
        let mut texture = 0;
        let mut calories = 0;
        for i in 0 .. ingredients.len() {
            capacity += list[i] as isize * ingredients[i].capacity;
            durability += list[i] as isize * ingredients[i].durability;
            flavor += list[i] as isize  * ingredients[i].flavor;
            texture += list[i] as isize * ingredients[i].texture;
            calories += list[i] as isize * ingredients[i].calories;
        }
        let capcity = not_negative(capacity);
        let durability = not_negative(durability);
        let flavor = not_negative(flavor);
        let texture = not_negative(texture);
        let total = capcity * durability * flavor * texture;
        max1 = std::cmp::max(max1, total);
        if calories == 500 { max2 = std::cmp::max(max2, total); }

        // --- Calculate next recipe ---
        let mut i = last - 1;
        let mut carry = true;
        while carry {
            let mut limit = MAX;
            if i > 0 {
                for j in 0 .. i { limit -= list[j]; }
            }
            list[i] += 1;
            if list[i] > limit {
                list[i] = 0;
                if i > 0 { i -= 1; } else { break 'outer; }
            } else {
                carry = false;
            }
            list[last] = MAX;
            for j in 0 .. last { list[last] -= list[j]; }
        }
    }

    // --- Puzzle 1 ---
    println!("1. Total score of the highest-scoring cookie: {}", max1);

    // --- Puzzle 2 ---
    println!("2. Total score of the highest-scoring cookie: {}", max2);


    Ok(())
}
