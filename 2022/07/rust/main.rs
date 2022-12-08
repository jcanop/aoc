use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::{ BufRead, BufReader };
use std::rc::Rc;
use std::str::FromStr;

// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";
const LIMIT: usize = 100_000;
const DISK_SIZE: usize = 70_000_000;
const UPDATE_SIZE: usize = 30_000_000;

// Enumeration that represents a line in the script.
#[derive(Debug)]
enum Line {
    CD { name: String },
    LS,
    DIR { name: String },
    FILE { name: String, size: usize }
}

impl FromStr for Line {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("$ cd ") { return Ok(Line::CD { name: String::from(&s[5..]) }); }
        if s.starts_with("$ ls") { return Ok(Line::LS); }
        if s.starts_with("dir ") { return Ok(Line::DIR { name: String::from(&s[4..]) }); }
        let i = s.find(" ").unwrap();
        let size = &s[0..i].parse::<usize>()?;
        let name = String::from(&s[i + 1..]);
        Ok(Line::FILE { name, size: *size })
    }
}

// Node RC type.
type RcNode = Rc<RefCell<Node>>;

// Node of the tree.
struct Node {
    #[allow(unused)]
    pub name: String,
    pub size: usize,
    pub parent: Option<RcNode>,
    pub childs: Option<HashMap<String, RcNode>>
}

impl Node {
    fn new_root() -> Self {
        Node { name: "/".to_string(), size: 0, parent: None, childs: Some(HashMap::new()) }
    }

    fn new_dir(parent: RcNode, name: String) -> Self {
        Node { name, size: 0, parent: Some(parent), childs: Some(HashMap::new()) }
    }

    fn new_file(parent: RcNode, name: String, size: usize) -> Self {
        Node { name, size, parent: Some(parent), childs: None }
    }

    // This recursive method updates all the directories sizes according to their content.
    fn update_dir_sizes(&mut self) {
        if let Some(childs) = &self.childs {
            for c in childs.values() {
                if c.borrow().childs.is_some() {
                    c.borrow_mut().update_dir_sizes();
                }
                self.size += c.borrow().size;
            }
        }
    }

    // This recursive method searches all directories smaller in size than a limit.
    fn sum_small_dirs(&self, limit: usize) -> usize {
        let mut total: usize = 0;
        if let Some(childs) = &self.childs {
            for c in childs.values() {
                let c = c.borrow();
                if c.childs.is_some() {
                    if c.size <= limit { total += c.size; }
                    total += c.sum_small_dirs(limit);
                }
            }
        }
        total
    }

    // This recursive method searches for the smaller directory over a limit.
    fn find_dir_to_delete(&self, need: usize) -> Option<usize> {
        let mut result: Option<usize> = None;
        if let Some(childs) = &self.childs {
            for c in childs.values() {
                let c = c.borrow();
                if c.childs.is_some() {
                    let res = c.find_dir_to_delete(need);
                    if res.is_some() && (result.is_none() || result.unwrap() > res.unwrap()) {
                        result = res;
                    }
                }
                if c.size >= need && (result.is_none() || result.unwrap() > c.size) {
                    result = Some(c.size);
                }
            }
        }
        result
    }


    // This recursive method prints a node and every child it has.
    #[allow(unused)]
    fn print(&self, level: usize) {
        for _ in 0..level { print!("  "); }
        println!("{} - {}", self.name, self.size);
        if let Some(childs) = &self.childs {
            for c in childs.values() {
                c.borrow().print(level + 1);
            }
        }
    }
}

fn main() -> Result<(), std::io::Error> {
    let root = Rc::new(RefCell::new(Node::new_root()));
    let mut current = Rc::clone(&root);

    // --- Read and parse the input file ---
    let file = File::open(INPUT_FILE)?;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        match line.parse().unwrap() {
            Line::CD { name } => match name.as_str() {
                "/"  => current = Rc::clone(&root),
                ".." => {
                    let current_clone = Rc::clone(&current);
                    current = Rc::clone(current_clone.borrow().parent.as_ref().unwrap());
                },
                _ => {
                    let current_clone = Rc::clone(&current);
                    current = Rc::clone(current_clone.borrow()
                        .childs.as_ref().unwrap().get(&name).unwrap());
                }
            },
            Line::LS => (),
            Line::DIR { name } => {
                let parent = Rc::clone(&current);
                let name = name.to_string();
                let dir = Rc::new(RefCell::new(Node::new_dir(parent, name.clone())));
                current.borrow_mut().childs.as_mut().unwrap().insert(name, Rc::clone(&dir));
            },
            Line::FILE { name, size } => {
                let parent = Rc::clone(&current);
                let name = name.to_string();
                let file = Rc::new(RefCell::new(Node::new_file(parent, name.clone(), size)));
                current.borrow_mut().childs.as_mut().unwrap().insert(name, Rc::clone(&file));
            }
        }
    }
    root.borrow_mut().update_dir_sizes();


    // --- Puzzle 1 ---
    let node = root.borrow();
    let total = node.sum_small_dirs(LIMIT);
    println!("1. The sum of the total sizes of the directories under {} is {}", LIMIT, total);

    // --- Puzzle 2 ---
    let size = node.find_dir_to_delete(UPDATE_SIZE - (DISK_SIZE - node.size));
    println!("2. The total size of the smalles directory needed is {}", size.unwrap());

    Ok(())
}
