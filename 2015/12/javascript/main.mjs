import fs from "node:fs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const REGEX = /(-?\d+)/g;

// --- Functions ---
function sum(json) {
	const type = typeof json;
	if (type === "number") return Number(json);
	if (type === "object") {
		if (Array.isArray(json)) return json.reduce((acc, x) => acc + sum(x), 0);
		if (Object.values(json).includes("red")) return 0;
		return Object.values(json).reduce((acc, x) => acc + sum(x), 0);
	}
	return 0;
}

// --- Parse the input file ---
const text = fs.readFileSync(INPUT_FILE, 'utf8').trim();

// --- Puzzle 1 ---
let total = text.match(REGEX).reduce((acc, x) => acc + Number(x), 0);
console.log("1. Sum of all numbers: ", total.toLocaleString());

// --- Puzzle 2 ---
const json = JSON.parse(text);
total = sum(json);
console.log("2. Sum of all numbers: ", total.toLocaleString());
