use std::fmt::{ Display, Formatter };

#[derive(PartialEq, Clone)]
pub enum Tile {
    Air, Origin, Rock, Sand
}

impl Display for Tile {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Tile::Air => write!(f, " "),
            Tile::Origin => write!(f, "+"),
            Tile::Rock => write!(f, "#"),
            Tile::Sand => write!(f, "o")
        }
    }
}
