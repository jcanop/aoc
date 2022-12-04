import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// Prase a Range from its string value
function parseRange(value) {
	const i = value.indexOf("-");
	const min = Number(value.substring(0, i));
	const max = Number(value.substring(i + 1));
	return { min, max }
}

//  Checks if this range fully contains another range.
function fullyContains(r1, r2) {
	return r1.min <= r2.min && r2.max <= r1.max;
}

// Checks if this range overlaps with another range.
function overlap(r1, r2) {
	return (r2.min >= r1.min && r2.min <= r1.max) ||
		(r2.max >= r1.min && r2.max <= r1.max) ||
		(r1.min >= r2.min && r1.min <= r2.max) ||
		(r1.max >= r2.min && r1.max <= r2.max);
}

// --- Parse the input file ---
let totals = [0, 0];
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	let i = line.indexOf(",");
	const r1 = parseRange(line.substring(0, i));
	const r2 = parseRange(line.substring(i + 1));

	// --- Puzzle 1 ---
	if (fullyContains(r1, r2) || fullyContains(r2, r1)) totals[0]++;

	// --- Puzzle 2 ---
	if (overlap(r1, r2)) totals[1]++;

});
await new Promise(res => reader.once("close", res));
console.log("Part 1. Total of ranges that fully contains another: ", totals[0].toLocaleString());
console.log("Part 2. Total of ranges that overlaps: ", totals[1].toLocaleString());
