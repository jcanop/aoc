import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const REGEX = /^([a-zA-Z]+) => ([a-zA-Z]+)$/;

// --- Functions ---
function shuffle(a) {
	for (let index = a.length - 1; index > 0; index--) {
		const j = Math.floor(Math.random() * (index + 1));
		const x = a[index];
		a[index] = a[j];
		a[j] = x;
	}
	return a;
}

// --- Variables ---
let map = {};
let pairs = [];
let molecule = "";
let first = true;

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	if (line.length === 0) {
		first = false;
	} else {
		if (first) {
			let cap = line.match(REGEX);
			if (!map.hasOwnProperty(cap[1])) map[cap[1]] = [];
			map[cap[1]].push(cap[2]);
			pairs.push(cap[1] + "-" + cap[2]);
		} else {
			molecule = line;
		}
	}
});
await new Promise(res => reader.once("close", res));

// --- Puzzle 1 ---
const regex = new RegExp("(" + Object.keys(map).join("|") + ")", "g");
let set = new Set();
let cap;
while ((cap = regex.exec(molecule)) !== null) {
	const i = cap.index;
	const j = i + cap[1].length;
	for (const x of map[cap[1]]) {
		const s = molecule.substring(0, i) + x + molecule.substring(j);
		set.add(s);
	}
}
console.log("1. Distinct molecules can be created: ", set.size.toLocaleString());

// --- Puzzle 2 ---
let target = molecule.slice();
let steps = 0;
while (target !== "e") {
	let change = false;
	for (const x of pairs) {
		const t = x.split("-");
		const from = t[0];
		const to = t[1];
		if (target.includes(to)) {
			target = target.replace(to, from);
			change = true;
			break;
		}
	}

	// --- If not change was possible shuffle pairs and try again ---
	if (!change) {
		shuffle(pairs);
		target = molecule.slice();
		steps = 0;
		continue;
	}

	steps++;
}
console.log("2. Fewest number of steps: ", steps.toLocaleString());
