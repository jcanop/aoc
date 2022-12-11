use std::fs::File;
use std::io::{ BufRead, BufReader };
use std::str::FromStr;

// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";

// --- Commands ---
struct NoOp {
    cycles: usize
}

impl NoOp {
    fn new() -> Self {
        NoOp { cycles: 1 }
    }
}

struct AddX {
    cycles: usize,
    value: isize
}

impl AddX {
    fn new(value: isize) -> Self {
        AddX { cycles: 2, value }
    }
}

enum Command {
    NoOp(NoOp),
    AddX(AddX)
}

// --- Trait for structs that reacts to ticks ---
trait Tickable {
    fn tick(&mut self) -> bool;
}

impl Tickable for Command {
    fn tick(&mut self) -> bool {
        match self {
            Command::NoOp(cmd) => { cmd.cycles -= 1; cmd.cycles > 0 }
            Command::AddX(cmd) => { cmd.cycles -= 1; cmd.cycles > 0 }
        }
    }
}

impl FromStr for Command {
    type Err = std::num::ParseIntError;

    fn from_str(s : &str) -> Result<Self, Self::Err> {
        if s == "noop" { return Ok(Command::NoOp(NoOp::new())); }
        if s.starts_with("addx ") {
            let value: isize = s[5..].parse()?;
            return Ok(Command::AddX(AddX::new(value)));
        }
        panic!("Unknown command: {}", s);
    }
}

// Represents the CPU.
struct CPU {
    program: Vec<Command>,
    register: isize
}

impl CPU {
    fn new(program: Vec<Command>) -> Self {
        CPU { program, register: 1 }
    }

    // Process a cycle.
    fn tick(&mut self) -> bool {
        if self.program.len() == 0 { return false; }
        let cmd = &mut self.program[0];
        if cmd.tick() { return true; }
        match cmd {
            Command::NoOp(_) => (),
            Command::AddX(cmd) => self.register += cmd.value
        }
        self.program.remove(0);
        self.program.len() > 0
    }
}

fn main() -> std::io::Result<()> {
    // --- Reads and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    let program: Vec<Command> = BufReader::new(file).lines().map(|line| line.unwrap().parse().unwrap()).collect();

    // --- Execute the program in the CPU --
    let mut cpu = CPU::new(program);
    let mut cycle = 1;
    let mut mark = 20;
    let mut total = 0;
    let mut buffer = String::new();

    loop {
        // --- Puzzle 1 --
        if cycle == mark {
            total += cpu.register * mark;
            mark += 40;
        }

        // --- Puzzle 2 ---
        let p = (cycle - 1) % 40;
        let c = if p >= cpu.register - 1 && p <= cpu.register + 1 { "#" } else { " " };
        buffer.push_str(c);
        if cycle % 40 == 0 { buffer.push_str("\n"); }

        // --- Both ---
        cycle += 1;
        if !cpu.tick() { break; }
    }

    // --- Puzzle 1 ---
    println!("1. The sum of the signal strenghts is: {}", total);

    // --- Puzzle 2 ---
    println!();
    println!("2. Image on the CRT");
    println!("{}", &buffer);

    Ok(())
}
