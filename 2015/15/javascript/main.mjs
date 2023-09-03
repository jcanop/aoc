import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const REGEX = /^[a-zA-Z]+: capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)$/;
const MAX = 100;

// --- Functions ---
function notNegative(value) {
	if (value < 0) return 0;
	return value;
}

// --- Variables ---
let ingredients = [];
let max1 = Number.MIN_VALUE;
let max2 = Number.MIN_VALUE;

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	const cap = line.match(REGEX);
	const capacity = Number(cap[1]);
	const durability = Number(cap[2]);
	const flavor = Number(cap[3]);
	const texture = Number(cap[4]);
	const calories = Number(cap[5]);
	ingredients.push({capacity, durability, flavor, texture, calories});
});
await new Promise(res => reader.once("close", res));

// --- Calculate combinations ---
let list = Array.from({length: ingredients.length - 1}, () => 0);
list.push(MAX);

let last = list.length - 1;
outer: while (list[0] < MAX) {
	let capacity = 0;
	let durability = 0;
	let flavor = 0;
	let texture = 0;
	let calories = 0;
	for (let i = 0; i < ingredients.length; i++) {
		capacity += list[i] * ingredients[i].capacity;
		durability += list[i] * ingredients[i].durability;
		flavor += list[i] * ingredients[i].flavor;
		texture += list[i] * ingredients[i].texture;
		calories += list[i] * ingredients[i].calories;
	}
	capacity = notNegative(capacity);
	durability = notNegative(durability);
	flavor = notNegative(flavor);
	texture = notNegative(texture);
	let total = capacity * durability * flavor * texture;
	max1 = Math.max(max1, total);
	if (calories === 500) max2 = Math.max(max2, total);

	// --- Calcualte next recipe ---
	let i = last - 1;
	let carry = true;
	while (carry) {
		let limit = MAX;
		if (i > 0) for (let j = 0; j < i; j++) limit -= list[i];
		list[i] += 1;
		if (list[i] > limit) {
			list[i] = 0;
			if (i > 0) { i--; } else { break  outer; }
		} else {
			carry = false;
		}
		list[last] = MAX;
		for (let j = 0; j < last; j++) list[last] -= list[j];
	}
}

// --- Puzzle 1 ---
console.log("1. Total score of the highest-scoring cookie: ", max1.toLocaleString());

// --- Puzzle 2 ---
console.log("2. Total score of the highest-scoring cookie: ", max2.toLocaleString());
