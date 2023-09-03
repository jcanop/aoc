import fs from "node:fs";
import readline from "node:readline";
import combinationN from "../../../commons/javascript/combinator.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Functions ---
function quantum_entanglement(list, groups) {
	const target = list.reduce((acc, x) => acc + x) / groups;
	let min = Number.MAX_VALUE;

	for (let i = 1; i < list.length; i++) {
		for (const c of combinationN(list, i)) {
			if (c.reduce((acc, x) => acc + x) === target) {
				min = Math.min(min, c.reduce((acc, x) => acc * x));
			}
		}
		if (min != Number.MAX_VALUE) return min;
	}
	throw "Not found!";
}

// --- Variables ---
let list = [];

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	list.push(Number(line));
});
await new Promise(res => reader.once("close", res));

// --- Puzzle 1 ---
let qe = quantum_entanglement(list, 3);
console.log("1. Quantum entaglement: ", qe.toLocaleString());

// --- Puzzle 2 ---
qe = quantum_entanglement(list, 4);
console.log("2. Quantum entaglement: ", qe.toLocaleString());
