import { loadMap, sim } from "./map.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Read and parse the input file ---
let map = await loadMap(INPUT_FILE);

// --- Puzzle 1 ---
sim(map, 1);
let total = map.countSandTiles();
console.log("1. Total units of sand:", total.toLocaleString());

// --- Puzzle 2 ---
sim(map, 2);
total = map.countSandTiles();
console.log("2. Total units of sand:", total.toLocaleString());
