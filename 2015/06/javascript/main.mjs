import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const LEN = 1000;
const REGEX = /(toggle|turn on|turn off) (\d+),(\d+) through (\d+),(\d+)/;

// --- Variables ---
let map1 = Array(LEN).fill().map(() => Array(LEN).fill(false));
let map2 = Array(LEN).fill().map(() => Array(LEN).fill(0));

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	const cap = line.match(REGEX);
	let op = cap[1];
	let x1 = Number(cap[2]);
	let y1 = Number(cap[3]);
	let x2 = Number(cap[4]);
	let y2 = Number(cap[5]);

	for (let y = y1; y <= y2; y++) {
		for (let x = x1; x <= x2; x++) {
			if (op === "toggle")   { map1[x][y] = !map1[x][y]; map2[x][y] += 2; }
			else if (op === "turn on")  { map1[x][y] = true; map2[x][y]++; }
			else if (op === "turn off") { map1[x][y] = false; if (map2[x][y] > 0) map2[x][y]--; }
			else console.log("Unsupported op: ", op);
		}
	}

});
await new Promise(res => reader.once("close", res));

// --- Puzzle 1 ---
let count = map1.reduce((acc1, a) => acc1 + a.reduce((acc2, v) => acc2 + (v ? 1 : 0), 0), 0);
console.log("1. Lights lit: ", count.toLocaleString());

// --- Puzzle 2 ---
count = map2.reduce((acc1, a) => acc1 + a.reduce((acc2, v) => acc2 + v, 0), 0);
console.log("2. Total brightness: ", count.toLocaleString());
