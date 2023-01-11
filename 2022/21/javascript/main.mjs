import fs from "node:fs";
import Resolver from "./resolver.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const ROOT = "root";
const ME = "humn";

// --- Read and parse the input file ---
const text = fs.readFileSync(INPUT_FILE, "utf8");

// --- Puzzle 1 ---
let resolver = new Resolver(text);
let result = resolver.solveFor(ROOT);
console.log("1. Number that will the monkey root yell:", result.toLocaleString());

// --- Puzzle 2 ---
resolver = new Resolver(text);
resolver.updateForPuzzle2(ROOT, ME);
result = resolver.solveFor(ME);
console.log("2. Number that I yell to pass root's equality test:", result.toLocaleString());
