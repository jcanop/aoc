import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Functions ---
function countMem(s) {
	let total = s.length - 2;
	const a = Array.from(s);
	let i = 1;
	while (i < a.length - 2) {
		if (a[i] === '\\') {
			if (a[i + 1] === '\\' || a[i + 1] == '"') {
				total--;
				i++;
			} else if (a[i + 1] === 'x') {
				total -= 3;
				i += 3;
			}
		}
		i++;
	}
	return total;
}

function encode(s) {
	let ls = ['"'];
	for (const c of Array.from(s)) {
		if (c === '"' || c === '\\') ls.push('\\');
		ls.push(c);
	}
	ls.push('"');
	return ls.join("");
}

// --- Variables ---
let total1 = 0;
let total2 = 0;

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	total1 += line.length - countMem(line);
	let enc = encode(line);
	total2 += enc.length - countMem(enc);
});
await new Promise(res => reader.once("close", res));

// --- Puzzle 1 ---
console.log("1. Number of characters in memory: ", total1.toLocaleString());

// --- Puzzle 2 ---
console.log("2. Number of characters in memory: ", total2.toLocaleString());
