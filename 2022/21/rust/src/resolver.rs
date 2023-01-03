use crate::Operation;
use std::collections::HashMap;
use std::fs::File;
use std::io::{ BufRead, BufReader };
use regex::Regex;

pub struct Resolver {
    queue: Vec<Operation>,
    solved: HashMap<String, u64>
}

impl Resolver {
    pub fn load(filename: &str) -> std::io::Result<Self> {
        let regex = Regex::new(r"\w+:\s+\d+").unwrap();
        let mut queue = Vec::new();
        let mut solved = HashMap::new();
        let file = File::open(filename)?;
        for line in BufReader::new(file).lines() {
            let line = line.unwrap();
            if regex.is_match(&line) {
                let i = line.find(':').unwrap();
                let key = line[0..i].to_string();
                let value: u64 = line[i + 2..].parse().unwrap();
                solved.insert(key, value);
            } else {
                let operation = line.parse().unwrap();
                queue.push(operation);
            }
        }
        Ok(Self { queue, solved })
    }

    pub fn update_for_puzzle2 (&mut self, root: &str, me: &str) {
        for op in self.queue.iter_mut() {
            if op.key == root {
                op.operator = '=';
            }
            if op.op1 == me { op.solve_for_variable(1); }
            else if op.op2 == me { op.solve_for_variable(2); }
        }
        self.solved.remove(me);
    }

    pub fn solve_for(mut self, id: &str) -> u64 {
        while let Some(op) = self.queue.pop() {
            if op.operator == '=' {
                let v1 = self.solved.get(&op.op1);
                let v2 = self.solved.get(&op.op2);
                if v1.is_some() && v2.is_none() {
                    self.add_solution(&op.op2, *v1.unwrap());
                } else if v1.is_none() && v2.is_some() {
                    self.add_solution(&op.op1, *v2.unwrap());
                }
            }

            let v1 = self.solved.get(&op.op1);
            let v2 = self.solved.get(&op.op2);
            if v1.is_some() && v2.is_some() {
                let result = match op.operator {
                    '+' => *v1.unwrap() + *v2.unwrap(),
                    '-' => *v1.unwrap() - *v2.unwrap(),
                    '*' => *v1.unwrap() * *v2.unwrap(),
                    '/' => *v1.unwrap() / *v2.unwrap(),
                    '=' => 0,
                    _ => panic!("Unsupported operation: {}", op.operator)
                };
                if op.operator != '=' {
                    self.add_solution(&op.key, result);
                }
            } else {
                self.queue.insert(0, op);
            }
        }
        self.solved[id]
    }

    fn add_solution(&mut self, key: &str, value: u64) {
        self.solved.insert(key.to_owned(), value);
        for op in self.queue.iter_mut().filter(|x| x.key == key) {
            let v1 = self.solved.get(&op.op1);
            let v2 = self.solved.get(&op.op2);
            if v1.is_some() && v2.is_none() { op.solve_for_variable(2); }
            else if v1.is_none() && v2.is_some() { op.solve_for_variable(1); }
        }
    }
}
