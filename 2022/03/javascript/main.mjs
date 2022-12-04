import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// Find the duplicated character on the arrays.
function findDuplicate(sa, sb, sc) {
	const a = sa.split("");
	const b = sb.split("");
	const c = sc ? sc.split("") : undefined;
	for (let i = 0; i < a.length; i++)
		for (let j = 0; j < b.length; j++)
			if (a[i] === b[j]) {
				if (c === undefined) return a[i];
				for (let k = 0; k < c.length; k++)
					if (b[j] === c[k])
						return c[k];
			}
}

// Get the priority value of a character.
function getPriority(c) {
	if (c >= "a" && c <= "z") return c.charCodeAt(0) - 'a'.charCodeAt(0) + 1;
	if (c => "A" && c <= "Z") return c.charCodeAt(0) - 'A'.charCodeAt(0) + 27;
	return undefined;
}

// --- Parse the input file ---
let totals = [0 , 0];
let lines = [];
let index = 0;
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	// --- Puzzle 1 ---
	const len = line.length / 2;
	const s1 = line.substring(0, len);
	const s2 = line.substring(len);
	const c = findDuplicate(s1, s2);
	totals[0] += getPriority(c);

	// --- Puzzle 2 ---
	lines[index++] = line;
	if (index === 3) {
		const d = findDuplicate(lines[0], lines[1], lines[2]);
		totals[1] += getPriority(d);
		index = 0;
	}
});
await new Promise(res => reader.once("close", res));
console.log("Part 1. Total sum of the priorities: ", totals[0].toLocaleString());
console.log("Part 2. Total sum of the item types: ", totals[1].toLocaleString());
