import createLayout from "./layout.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Read and parse the input file ---
let layout = createLayout(INPUT_FILE);

// --- Puzzle 1 ---
let total = layout.dfs1();
console.log("1. Total pressure released:", total);

// --- Puzzle 2 ---
total = layout.dfs2();
console.log("2. Total pressure released with an elephant:", total);
