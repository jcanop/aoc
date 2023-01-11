import fs from "node:fs";
import Map from "./map.mjs";
import { moveGrid, moveCube } from "./lib.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Read the input file ---
const text = fs.readFileSync(INPUT_FILE, "utf8");
const i = text.indexOf("\n\n");
const data = text.substring(0, i);
const code = text.substring(i + 2);

// --- Puzzle 1 ---
let map = new Map(data);
map.path(code, moveGrid);
let password = map.getPassword();
console.log("1. Password:", password.toLocaleString());

// --- Puzzle 2 ---
map = new Map(data);
map.path(code, moveCube);
password = map.getPassword();
console.log("2. Password:", password.toLocaleString());
