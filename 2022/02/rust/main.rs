use std::fs::File;
use std::io::{ BufRead, BufReader };
use std::str::FromStr;

const INPUT_FILE:&'static str = "../input/input.txt";

//
// This enumeration represents the result of a game.
//
#[derive(Debug)]
enum GameResult { Win, Draw, Lose }

impl FromStr for GameResult {
    type Err = std::string::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let element = match s {
            "X" => GameResult::Lose,
            "Y" => GameResult::Draw,
            "Z" => GameResult::Win,
            _ => panic!("Illegal argument {}", s)
        };
        Ok(element)
    }
}

//
// This enumeration represents the shapes of the game: rock, paper, scissors.
//
#[derive(PartialEq, Clone, Debug)]
enum Shape { Rock, Paper, Scissors }

impl Shape {
    // This method returns the shape to win, lose or draw against this shape.
    pub fn shape_to(&self, result: &GameResult) -> Shape {
        match result {
            GameResult::Win => match self {
                Shape::Rock => Shape::Paper,
                Shape::Paper => Shape::Scissors,
                Shape::Scissors => Shape::Rock
            },
            GameResult::Lose => match self {
                Shape::Rock => Shape::Scissors,
                Shape::Paper => Shape::Rock,
                Shape::Scissors => Shape::Paper
            },
            GameResult::Draw => self.clone()
        }
    }

    // This method returs the points gained for using this shape.
    fn points(&self) -> usize {
        match self {
            Shape::Rock => 1,
            Shape::Paper => 2,
            Shape::Scissors => 3
        }
    }

    // This method matches this shape agains another  and returns a result.
    fn do_match(&self, opponent: &Shape) -> GameResult {
        if self == opponent { return GameResult::Draw; }
        if (self == &Shape::Rock && opponent == &Shape::Scissors) ||
            (self == &Shape::Scissors && opponent == &Shape::Paper) ||
            (self == &Shape::Paper && opponent == &Shape::Rock) { return GameResult::Win; }
        GameResult::Lose
    }

    // This method plays the game against this shape.
    pub fn play(&self, opponent: &Shape) -> usize {
        self.points() + match self.do_match(opponent) {
            GameResult::Win => 6,
            GameResult::Draw => 3,
            GameResult::Lose => 0
        }
    }
}

impl FromStr for Shape {
    type Err = std::string::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let element = match s {
            "A" | "X" => Shape::Rock,
            "B" | "Y" => Shape::Paper,
            "C" | "Z" => Shape::Scissors,
            _ => panic!("Illegal argument {}", s)
        };
        Ok(element)
    }
}

fn main() -> Result<(), std::io::Error> {
    // --- Puzzle: Part 1 ---
    let file = File::open(INPUT_FILE)?;
    let total: usize = BufReader::new(file).lines().map(|line| {
        let line = line.unwrap();
        let op: Shape = line[0..1].parse().unwrap();
        let me: Shape = line[2..3].parse().unwrap();
        me.play(&op)
    }).sum();
    println!("Part 1. Total score: {}", total);

    // --- Puzzle: Part 2 ---
    let file = File::open(INPUT_FILE)?;
    let total: usize = BufReader::new(file).lines().map(|line| {
        let line = line.unwrap();
        let op: Shape = line[0..1].parse().unwrap();
        let result: GameResult = line[2..3].parse().unwrap();
        let me: Shape = op.shape_to(&result);
        me.play(&op)
    }).sum();
    println!("Part 2. Total score: {}", total);

    Ok(())
}
