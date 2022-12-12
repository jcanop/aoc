use regex::Regex;
use std::fmt::{ Display, Formatter };

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = "Monkey (?:(\\d+)):\\s+\
        Starting items: (?:([\\d, ]+))\\s+\
        Operation: new = old (?:([\\+\\*])) (?:(\\d+|old))\\s+\
        Test: divisible by (?:(\\d+))\\s+\
        If true: throw to monkey (?:(\\d+))\\s+\
        If false: throw to monkey (?:(\\d+))";

// Represents a monkey in the game.
struct Monkey {
    id: usize,
    items: Vec<u64>,
    operator: char,
    operand: Option<u64>,
    divisible: u64,
    throw_true: usize,
    throw_false: usize,
    relief: usize,
    inspects: usize,
    gcd: u64
}

impl Monkey {
    fn new(id: usize, items: Vec<u64>, operator: char, operand: Option<u64>, divisible: u64, throw_true: usize, throw_false: usize, relief: usize) -> Self {
        Self { id, items, operator, operand, divisible, throw_true, throw_false, relief, inspects: 0, gcd: 0 }
    }

    fn play(&mut self) -> Vec<(usize, u64)> {
        let mut throws = Vec::new();
        while self.items.len() > 0 {
            self.inspects += 1;
            let mut item = self.items.remove(0);
            let value = if self.operand.is_none() { item } else { self.operand.unwrap() };
            match self.operator {
                '+' => item += value,
                '*' => item *= value,
                _ => panic!("Unsupported operator: {}", self.operator)
            }
            match self.relief {
                1 => item /= 3,
                2 => item %= self.gcd,
                _ => panic!("Unsupported relief: {}", self.relief)
            }
            let index = if item % self.divisible == 0 { self.throw_true } else { self.throw_false };
            throws.push((index, item));
        }
        throws
    }
}

impl Display for Monkey {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "[Monkey] id: {},  items: {:?}, inspects: {}", self.id, self.items, self.inspects)
    }
}

fn parse_input_file(relief: usize) -> Result<Vec<Monkey>, Box<dyn std::error::Error>> {
    let mut group: Vec<Monkey> = Vec::new();
    let text = std::fs::read_to_string(INPUT_FILE)?;
    let regex = Regex::new(PARSER_REGEX)?;
    for cap in regex.captures_iter(&text) {
        let id: usize = cap[1].parse()?;
        let items: Vec<u64> = cap[2].split(",").map(|x| x.trim().parse::<u64>().unwrap()).collect();
        let operator: char = cap[3].chars().next().unwrap();
        let operand: Option<u64> = if &cap[4] == "old" { None } else { Some(cap[4].parse()?) };
        let divisible: u64 = cap[5].parse()?;
        let throw_true: usize = cap[6].parse()?;
        let throw_false: usize = cap[7].parse()?;
        let monkey = Monkey::new(id, items, operator, operand, divisible, throw_true, throw_false, relief);
        group.push(monkey);
    }
    Ok(group)
}

fn calculate_monkey_business(group: &Vec<Monkey>) -> u64 {
    let mut v = group.iter().map(|m| m.inspects).collect::<Vec<usize>>();
    v.sort();
    v.reverse();
    v.into_iter().take(2).fold(1, |acc, x| acc * x as u64)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Puzzle 1 ---
    let mut group = parse_input_file(1)?;
    for _ in 0..20 {
        for i in 0..group.len() {
            let mut monkey = group.get_mut(i).unwrap();
            let throws = monkey.play();
            for (i, item) in throws.into_iter() {
                monkey = group.get_mut(i).unwrap();
                monkey.items.push(item);
            }
        }
    }
    let mb = calculate_monkey_business(&group);
    println!("1. Level of monkey business: {}", mb);

    // --- Puzzle 2 ---
    let mut group = parse_input_file(2)?;
    let gcd = group.iter().map(|m| m.divisible as u64).fold(1, |acc, x| acc * x);
    for monkey in group.iter_mut() {
        monkey.gcd = gcd;
    }

    for _ in 0..10_000 {
        for i in 0..group.len() {
            let mut monkey = group.get_mut(i).unwrap();
            let throws = monkey.play();
            for (i, item) in throws.into_iter() {
                monkey = group.get_mut(i).unwrap();
                monkey.items.push(item);
            }
        }
    }
    let mb = calculate_monkey_business(&group);
    println!("2. Level of monkey business: {}", mb);

    Ok(())
}
