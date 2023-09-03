import fs from "node:fs";
import readline from "node:readline";
import combinationN from "../../../commons/javascript/combinator.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const LITERS = 150

// --- Variables ---
let list = [];
let count = 0;
let map = {};

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	list.push((Number(line)));
});
await new Promise(res => reader.once("close", res));

// --- Combinations ---
for (let i = 1; i <= list.length; i++) {
	for (const c of combinationN(list, i)) {
		let total = c.reduce((acc, x) => acc + x);
		if (total == LITERS) {
			count++;
			const len = c.length;
			if (!map.hasOwnProperty(len)) map[len] = 0;
			map[len]++;
		}
	}
}

// --- Puzzle 1 ---
console.log("1. Combinations of containers: ", count.toLocaleString());

// --- Puzzle 2 ---
const min = Math.min.apply(null, Object.keys(map).map(x => Number(x)));
console.log("2. Number of different ways: ", map[min].toLocaleString());
