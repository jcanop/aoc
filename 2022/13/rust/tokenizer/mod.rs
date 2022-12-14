// --- Tokenizer ---
// Divide the string into tokens

pub struct Tokenizer {
    list: Vec<char>
}

impl Tokenizer {
    pub fn new(s: &str) -> Self {
        let list: Vec<char> = s[1..s.len() - 1].chars().collect();
        Self { list }
    }
}

impl IntoIterator for Tokenizer {
    type Item = String;
    type IntoIter = TokenizerIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        TokenizerIntoIter {
            tokenizer: self,
            start: 0,
            end: 0,
            count: 0
        }
    }
}

pub struct TokenizerIntoIter {
    tokenizer: Tokenizer,
    start: usize,
    end: usize,
    count: usize
}

impl TokenizerIntoIter {
    fn create_token(&mut self, skip: usize) -> Option<String> {
        let s: String = self.tokenizer.list[self.start..self.end - skip].iter().collect();
        self.start = self.end;
        Some(s)
    }
}

impl Iterator for TokenizerIntoIter {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        while self.end < self.tokenizer.list.len() {
            let c = self.tokenizer.list[self.end];
            self.end += 1;

            match c {
                '[' => self.count += 1,
                ']' => {
                    if self.count == 0 {
                        return self.create_token(0);
                    }
                    self.count -= 1;
                },
                ',' => {
                    if self.count == 0 {
                        return self.create_token(1);
                    }
                },
                _ => ()
            }
        }
        if self.end - self.start ==  0 {
            return None;
        }
        self.create_token(0)
    }
}
