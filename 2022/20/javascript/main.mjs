import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const KEY = 811589153;
const MIX_COUNT = 10;
const OFFSET_X = 1_000;
const OFFSET_Y = 2_000;
const OFFSET_Z = 3_000;

// --- Mix and Sum functions ---
function mix(list) {
	const len = list.length - 1;
	for (let id = 0; id < list.length; id++) {
		const i = list.findIndex(e => e.id === id);
		if (list[i].value === 0) continue;
		const item = list.splice(i, 1)[0];
		let j = (i + item.value) % len;
		if (j < 0) j = len + j;
		if (j === 0) j = len;
		list.splice(j, 0, item);
	}
}

function sum(list) {
	const i = list.findIndex(e => e.value === 0);
	const len = list.length;
	let x = list[(i + OFFSET_X) % len].value;
	let y = list[(i + OFFSET_Y) % len].value;
	let z = list[(i + OFFSET_Z) % len].value;
	return x + y + z;
}

// --- Read and parse the input file ---
let numbers = [];
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	numbers.push(Number(line));
});
await new Promise(res => reader.once("close", res));

// --- Puzzle 1 ---
let list = [];
for (const [id, value] of numbers.entries()) {
	list.push({ id, value });
}
mix(list);
let total = sum(list);
console.log("1. The sum of the coordinates:", total.toLocaleString());

// --- Puzzle 2 ---
list = [];
for (const [id, v] of numbers.entries()) {
	list.push({ id, value: v * KEY });
}
for (let i = 0; i < MIX_COUNT; i++) mix(list);
total = sum(list);
console.log("2. The sum of the coordinates:", total.toLocaleString());
