import fs from "node:fs";
import readline from "node:readline";
import permutations from "../../../commons/javascript/permutations.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const REGEX = /^(.+) to (.+) = (\d+)$/

// --- Variables ---
let places = new Set();
let distances = {};
let min = Number.MAX_VALUE;
let max = Number.MIN_VALUE;

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	const cap = line.match(REGEX);
	places.add(cap[1]);
	places.add(cap[2]);
	distances[cap[1] + "-" + cap[2]] = Number(cap[3]);
	distances[cap[2] + "-" + cap[1]] = Number(cap[3]);
});
await new Promise(res => reader.once("close", res));

// --- Calculate distances ---
places = Array.from(places);
for (const p of permutations(places)) {
	let distance = 0;
	for (let i = 0; i < p.length - 1; i++) {
		distance += distances[p[i] + "-" + p[i + 1]];
	}

	min = Math.min(min, distance);
	max = Math.max(max, distance);
}

// --- Results ---
console.log("1. Min distance: ", min.toLocaleString());
console.log("2. Max distance: ", max.toLocaleString());
