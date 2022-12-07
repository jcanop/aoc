use std::fs::File;
use std::io::{ Read, BufReader };

// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";
const C_PACKET: usize = 4;
const C_MESSAGE: usize = 14;

// Indicates if an array has unique characters.
fn unique(data: &[u8]) -> bool {
    for i in 0..data.len() {
        for j in 0..data.len() {
            if i != j && data[i] == data[j] { return false; }
        }
    }
    true
}

// Search the stream of data for a marker.
fn find(data: &[u8], c: usize) -> Option<usize> {
    let len = data.len();
    if len < c { return None; }
    for i in 0..(len - c) {
        if unique(&data[i..(i + c)]) {
            return Some(i + c);
        }
    }
    None
}

// Search for the start of a packet.
fn find_packet(data: &[u8]) -> Option<usize> {
    find(data, C_PACKET)
}

// Search for the start of a message.
fn find_message(data: &[u8]) -> Option<usize> {
    find(data, C_MESSAGE)
}

fn main() -> std::io::Result<()> {
    // --- Load the input file ---
    let f = File::open(INPUT_FILE)?;
    let mut buffer = Vec::new();
    let mut reader = BufReader::new(f);
    reader.read_to_end(&mut buffer)?;

    // --- Puzzles ---
    println!("Part 1. Start-of-packet marker: {}", find_packet(&buffer).unwrap());
    println!("Part 2. Start-of-message marker: {}", find_message(&buffer).unwrap());

    Ok(())
}
