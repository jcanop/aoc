import fs from "node:fs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Functions ---
function lookAndSay(s) {
	const a = Array.from(s);
	let res = [];
	let last = a[0];
	let count = 1;
	for (let i = 1; i < a.length; i++) {
		if (a[i] === last) count++;
		else {
			res.push(Array.from(count.toString()));
			res.push(last);
			count = 1;
		}
		last = a[i];
	}
	res.push(Array.from(count.toString()));
	res.push(last);
	return res.join("");
}

// --- Parse the input file ---
let text = fs.readFileSync(INPUT_FILE, 'utf8').trim();

// --- Puzzle 1 ---
for (let i = 0; i < 40; i++) text = lookAndSay(text);
console.log("1. Length of the result: ", text.length.toLocaleString());

// --- Puzzle 2 ---
for (let i = 0; i < 10; i++) text = lookAndSay(text);
console.log("2. Length of the result: ", text.length.toLocaleString());
