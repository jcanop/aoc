import fs from "node:fs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Parse the input file ---
const data = fs.readFileSync(INPUT_FILE, 'utf8');

// --- Puzzle 1 & 2 ---
var pos = 0;
var result = Array.from(data)
	.map((x, i) => [x, i])
	.reduce((acc, a) => {
		if (pos == 0 && acc == -1) pos = a[1];
		return acc + (a[0] == ')' ? -1 : 1);
	}, 0);

console.log("1. Floor: ", result.toLocaleString());
console.log("2. Position: ", pos.toLocaleString());
