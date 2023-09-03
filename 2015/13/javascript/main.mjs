import fs from "node:fs";
import readline from "node:readline";
import permutations from "../../../commons/javascript/permutations.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const REGEX = /(.+)? would (gain|lose) (\d+) happiness units by sitting next to (.+)\./;

// --- Functions ---
function calculate_happiness(persons, happiness) {
	let max = Number.MIN_VALUE;
	for (const p of permutations(persons)) {
		let h = 0;
		for (let i = 0; i < p.length; i++) {
			let n1 = p[i];
			let n2 = p[(i + 1) % p.length];
			h += happiness[n1 + "-" + n2];
			h += happiness[n2 + "-" + n1];
		}
		max = Math.max(max, h);
	}
	return max;
}

// --- Variables ---
let persons = new Set();
let happiness = {};

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	let cap = line.match(REGEX);
	persons.add(cap[1]);
	persons.add(cap[4]);
	let h = Number(cap[3]);
	if (cap[2] === "lose") h *= -1;
	happiness[cap[1] + "-" + cap[4]] = h;
});
await new Promise(res => reader.once("close", res));
persons = Array.from(persons);

// --- Puzzle 1 ---
let max = calculate_happiness(persons, happiness);
console.log("1. Max change in happiness: ", max.toLocaleString());

// --- Puzzle 2 ---
persons.push("me");
for (const p of persons) {
	happiness[p + "-me"] = 0;
	happiness["me-" + p] = 0;
}
max = calculate_happiness(persons, happiness);
console.log("2. Max change in happiness: ", max.toLocaleString());
