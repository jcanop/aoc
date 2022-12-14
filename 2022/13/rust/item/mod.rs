use tokenizer::Tokenizer;
use std::cmp::Ordering;
use std::fmt::{ Display, Formatter };
use std::str::FromStr;

#[derive(Eq, Ord)]
pub enum Item {
    IntItem(usize),
    ArrayItem(Vec<Item>)
}

impl FromStr for Item {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("[") {
            let mut list: Vec<Item> = vec![];
            let tokenizer: Tokenizer = Tokenizer::new(s);
            for token in tokenizer {
                let item: Item = token.parse()?;
                list.push(item);
            }
            Ok(Item::ArrayItem(list))
        } else {
            Ok(Item::IntItem(s.parse::<usize>()?))
        }
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Item::IntItem(n) => write!(f, "{}", n),
            Item::ArrayItem(list) => {
                write!(f, "[")?;
                for i in 0..list.len() {
                    let item = list.get(i).unwrap();
                    item.fmt(f)?;
                    if i < list.len() - 1 {
                        write!(f, ",")?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}

impl PartialEq for Item {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Item::IntItem(a) => match other {
                Item::IntItem(b) => a == b,
                Item::ArrayItem(_) => {
                    let a: Item = format!("[{}]", a).parse().unwrap();
                    a.eq(other)
                }
            },
            Item::ArrayItem(a) => match other {
                Item::IntItem(b) => {
                    let b: Item = format!("[{}]", b).parse().unwrap();
                    self.eq(&b)
                },
                Item::ArrayItem(b) => {
                    if a.len() != b.len() { return false; }
                    for i in 0..a.len() {
                        let r = a[i].eq(&b[i]);
                        if !r { return false; }
                    }
                    true
                }
            }
        }
    }
}

impl PartialOrd for Item {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            Item::IntItem(a) => match other {
                Item::IntItem(b) => compare_int(a, b),
                Item::ArrayItem(_) => {
                    let a: Item = format!("[{}]", a).parse().unwrap();
                    a.partial_cmp(other)
                }
            },
            Item::ArrayItem(a) => match other {
                Item::IntItem(b) => {
                    let b: Item = format!("[{}]", b).parse().unwrap();
                    self.partial_cmp(&b)
                },
                Item::ArrayItem(b) => compare_array(a, b)
            }
        }
    }
}

fn compare_int(a: &usize, b: &usize) -> Option<Ordering> {
    if a < b { Some(Ordering::Less) }
    else if a > b { Some(Ordering::Greater) }
    else { Some(Ordering::Equal) }
}

fn compare_array(a: &Vec<Item>, b: &Vec<Item>) -> Option<Ordering> {
    let len = std::cmp::max(a.len(), b.len());
    for i in 0..len {
        if i == a.len() { return Some(Ordering::Less) }
        if i == b.len() { return Some(Ordering::Greater) }
        if a[i] < b[i] { return Some(Ordering::Less) }
        if a[i] > b[i] { return Some(Ordering::Greater) }
    }
    Some(Ordering::Equal)
}
