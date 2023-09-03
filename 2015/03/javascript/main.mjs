import fs from "node:fs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Parse the input file ---
const data = fs.readFileSync(INPUT_FILE, 'utf8');

// --- Puzzle 1 & 2 ---
let p = { x: 0, y: 0 };
let s = { x: 0, y: 0 };
let r = { x: 0, y: 0 };
let set1 = new Set(["0,0"]);
let set2 = new Set(["0,0"]);
let n = 0;
for (const c of Array.from(data)) {
	n++;
	if (c == '^') { p.y--; if (n % 2 == 0) r.y--; else s.y--; }
	if (c == 'v') { p.y++; if (n % 2 == 0) r.y++; else s.y++; }
	if (c == '<') { p.x--; if (n % 2 == 0) r.x--; else s.x--; }
	if (c == '>') { p.x++; if (n % 2 == 0) r.x++; else s.x++; }

	set1.add(p.x + "," + p.y);
	set2.add(n % 2 == 0 ? r.x + "," + r.y : s.x + "," + s.y);
}

// --- Puzzle 1 ---
console.log("1. Houses with at least one present: {}", set1.size.toLocaleString());

// --- Puzzle 2 ---
console.log("2. Houses with at least one present: {}", set2.size.toLocaleString());
