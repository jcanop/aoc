import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const REGEX = /([a-z]+): (\d+)/g;

// --- MFCSAM Output ---
const MAP = {
	children: 3,
	cats: 7,
	samoyeds: 2,
	pomeranians: 3,
	akitas: 0,
	vizslas: 0,
	goldfish: 5,
	trees: 3,
	cars: 2,
	perfumes: 1
};

// --- Functions ---
function find1(line) {
	for (const cap of line.matchAll(REGEX)) {
		const key = cap[1];
		const value = Number(cap[2]);
		if (MAP.hasOwnProperty(key) && MAP[key] !== value) return false;
	}
	return true;
}

function find2(line) {
	for (const cap of line.matchAll(REGEX)) {
		const key = cap[1];
		const value = Number(cap[2]);
		if (MAP.hasOwnProperty(key)) {
			if (key === "cats" || key === "trees") {
				if (MAP[key] >= value) return false;
			} else if (key === "pomeranians" || key === "goldfish") {
				if (MAP[key] <= value) return false;
			} else {
				if (MAP[key] !== value) return false;
			}
		}
	}
	return true;
}

// --- Variables ---
let sue1 = null;
let sue2 = null;

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	// --- Puzzle 1 ---
	if (sue1 === null && find1(line)) {
		sue1 = line.split(":")[0];
	}

	// --- Puzzle 2 ---
	if (sue2 === null && find2(line)) {
		sue2 = line.split(":")[0];
	}
});
await new Promise(res => reader.once("close", res));

// --- Results ---
console.log("1. Number of the Sue that got you the gift: ", sue1);
console.log("2. Number of the Sue that got you the gift: ", sue2);
