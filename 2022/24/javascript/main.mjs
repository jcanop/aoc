import fs from "node:fs";
import Map from "./map.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Read and parse the input file ---
const text = fs.readFileSync(INPUT_FILE, "utf8");

// --- Puzzle 1 ---
let map = new Map(text);
const start = { x: 1, y: 0 };
const end = { x: map.width - 2, y: map.height - 1 };
let time = map.search(start, end, 0);
console.log("1. Minutes to reach the goal:", time.toLocaleString());

// --- Puzzle 2 ---
time = map.search(end, start, time);
time = map.search(start, end, time);
console.log("2. Minutes to reach the goal, go back to the start, then reach the goal again", time.toLocaleString());
