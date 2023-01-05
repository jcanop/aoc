pub mod blizzard;
pub mod map;

pub use blizzard::Blizzard;
pub use map::Map;

pub type Grid = Vec<Vec<char>>;
pub type Point = (usize, usize);
