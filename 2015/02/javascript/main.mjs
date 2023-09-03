import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const REGEX = /(\d+)x(\d+)x(\d+)/

// --- Variables ---
let total = 0;
let ribbon = 0;

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	let cap = line.match(REGEX);
	let l = Number(cap[1]);
	let w = Number(cap[2]);
	let h = Number(cap[3]);
	let a = l * w;
	let b = w * h;
	let c = l * h;
	let m = Math.min(a, Math.min(b, c));
	total += 2 * a + 2 * b + 2 * c + m;

	m = Math.max(l, Math.max(w, h));
	ribbon += 2 * (l + w + h - m) + l * w * h;
});
await new Promise(res => reader.once("close", res));

// --- Puzzle 1 ---
console.log("1. Total square feet of wrapping paper: ", total.toLocaleString());

// --- Puzzle 2 ---
console.log("2. total feet of ribbon: ", ribbon.toLocaleString());
