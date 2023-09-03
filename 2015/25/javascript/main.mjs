import fs from "node:fs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const REGEX = /Enter the code at row (\d+), column (\d+)/;
const FIRST_CODE = 20151125;

// --- Parse the input file ---
const input = fs.readFileSync(INPUT_FILE, 'utf8');
const cap = input.match(REGEX);
const row = Number(cap[1]);
const column = Number(cap[2]);

// --- Find the code ---
const n = row + column - 1;
const target = ((Math.pow(n, 2) + n) / 2) - (n - column);
let code = FIRST_CODE;
for (let i = 1; i < target; i++) code = (code * 252533) % 33554393;
console.log("Code: ", code.toLocaleString());
