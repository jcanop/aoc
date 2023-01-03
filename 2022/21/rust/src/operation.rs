use std::str::FromStr;

pub struct Operation {
    pub key: String,
    pub op1: String,
    pub op2: String,
    pub operator: char
}

impl Operation {
    pub fn solve_for_variable(&mut self, i: usize) {
        if i == 1 {
            self.operator = match self.operator {
                '+' => '-',
                '-' => '+',
                '*' => '/',
                '/' => '*',
                _ => panic!("Unsupported operator: {}", self.operator)
            };
            std::mem::swap(&mut self.key, &mut self.op1);
        } else if i == 2 {
            let mut swap = false;
            self.operator = match self.operator {
                '+' => { swap = true; '-' },
                '-' => { '-' },
                '*' => { swap = true; '/' },
                '/' => { '/' },
                _ => panic!("Unsupported operator: {}", self.operator)
            };
            std::mem::swap(&mut self.key, &mut self.op2);
            if swap {
                std::mem::swap(&mut self.op1, &mut self.op2);
            }
        } else {
            panic!("Unsupported i: {}", i);
        }
    }
}

impl FromStr for Operation {
    type Err = std::string::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split_ascii_whitespace();
        let key = tokens.next().unwrap();
        let key = key[0..key.len() - 1].to_string();
        let op1 = tokens.next().unwrap().to_string();
        let operator = tokens.next().unwrap().chars().nth(0).unwrap();
        let op2 = tokens.next().unwrap().to_string();
        Ok(Self { key, op1, op2, operator })
    }
}
