import fs from "node:fs";
import readline from "node:readline";

const INPUT_FILE = "../input/input.txt";

async function readFile(filename) {
	const stream = fs.createReadStream(filename);
	const reader = readline.createInterface({ input: stream, crlDelay: Infinity });

	let list = [];
	let total = 0;
	for await (const line of reader) {
		if (line.length === 0) {
			list.push(total);
			total = 0;
		} else {
			total += parseInt(line);
		}
	}
	list.push(total);
	return list;
}

// --- Read and parse the input file ---
let list = await readFile(INPUT_FILE);
list.sort((a, b) => b - a);

// --- Find the Elf carrying the most Calories ---
const max = list[0].toLocaleString();
console.log(`The Elf carrying the most Calories, is carrying ${max} Calories.`);

// --- Find the top three Elves carrying the most Calories ---
const total = list.slice(0, 3).reduce((acc, val) => acc += val).toLocaleString();
console.log(`The top 3 Elves carrying the most Calores, are carrying ${total} Calories`);
