use std::fs::File;
use std::io::{ BufRead, BufReader };
use std::str::FromStr;

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";

// --- Enums ---
#[derive(Debug)]
enum Register { A, B }

impl FromStr for Register {
    type Err = Box<dyn std::error::Error>;

    fn from_str(input: &str) -> Result<Register, Self::Err> {
        match &input[0..1] {
            "a" => Ok(Register::A),
            "b" => Ok(Register::B),
            _   => Err(format!("Invalid registry: {}", input).into())
        }
    }
}

#[derive(Debug)]
enum Instruction {
    Half(Register),
    Triple(Register),
    Increment(Register),
    Jump(isize),
    JumpIfEven(Register, isize),
    JumpIfOne(Register, isize)
}

impl FromStr for Instruction {
    type Err = Box<dyn std::error::Error>;

    fn from_str(input: &str) -> Result<Instruction, Self::Err> {
        let s = input.split(" ").collect::<Vec<&str>>();
        match s[0] {
            "hlf" => Ok(Instruction::Half(Register::from_str(s[1])?)),
            "tpl" => Ok(Instruction::Triple(Register::from_str(s[1])?)),
            "inc" => Ok(Instruction::Increment(Register::from_str(s[1])?)),
            "jmp" => Ok(Instruction::Jump(s[1].parse()?)),
            "jie" => Ok(Instruction::JumpIfEven(Register::from_str(s[1])?, s[2].parse()?)),
            "jio" => Ok(Instruction::JumpIfOne(Register::from_str(s[1])?, s[2].parse()?)),
            _   => Err(format!("Invalid instruction: {}", input).into())
        }
    }
}

// --- Structs ---
struct CPU {
    a: usize,
    b: usize
}

impl CPU {
    fn new() -> Self {
        CPU { a: 0, b: 0 }
    }

    fn exec(&mut self, code: &Vec<Instruction>) {
        let mut i = 0;
        while i < code.len() {
            match &code[i] {
                Instruction::Half(r) => {
                    let v = self.get(r) / 2;
                    self.set(r, v);
                    i += 1;
                },
                Instruction::Triple(r) => {
                    let v = self.get(r) * 3;
                    self.set(r, v);
                    i += 1;
                },
                Instruction::Increment(r) => {
                    let v = self.get(r) + 1;
                    self.set(r, v);
                    i += 1;
                },
                Instruction::Jump(n) => {
                    i = (i as isize + *n) as usize;
                },
                Instruction::JumpIfEven(r, n) => {
                    if self.get(r) % 2 == 0 {
                        i = (i as isize + *n) as usize;
                    } else {
                        i += 1;
                    }
                },
                Instruction::JumpIfOne(r, n) => {
                    if self.get(r) == 1 {
                        i = (i as isize + *n) as usize;
                    } else {
                        i += 1;
                    }

                }
            }
        }
    }

    fn get(&self, r: &Register) -> usize {
        match r {
            Register::A => self.a,
            Register::B => self.b
        }
    }

    fn set(&mut self, r: &Register, v: usize) {
        match r {
            Register::A => self.a = v,
            Register::B => self.b = v
        }
    }
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut code: Vec<Instruction> = Vec::new();

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        code.push(Instruction::from_str(&line)?);
    }

    // --- Puzzle 1 ---
    let mut cpu = CPU::new();
    cpu.exec(&code);
    println!("1. Register b: {}", cpu.b);

    // --- Puzzle 2 ---
    let mut cpu = CPU::new();
    cpu.a = 1;
    cpu.exec(&code);
    println!("2. Register b: {}", cpu.b);


    Ok(())
}
