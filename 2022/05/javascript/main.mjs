import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const CRATES_REGEX = /(\[\w\]|\s{3})\s?/g;
const STEPS_REGEX = /\d+/g;

let queues1 = [];
let queues2 = [];
let setup = true;

const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	// --- Parse Initial crates setup ---
	if (setup) {
		if (line.indexOf("[") >= 0) {
			let index = 0;
			for (let token of line.match(CRATES_REGEX)) {
				if (queues1.length - 1 < index) {
					queues1.push([]);
					queues2.push([]);
				}
				let c = token.charAt(1);
				if (c !== " ") {
					queues1[index].push(c);
					queues2[index].push(c);
				}
				index++;
			}
		}

		// --- An empty line marks ends of setup ---
		if (line.length === 0) setup = false;
	} else {
		// --- Parse script ---
		let [count, from, to] = line.match(STEPS_REGEX);
		from--;
		to--;

		// --- Puzzle 1 ---
		for (let i = 0; i < count; i++) {
			let crate = queues1[from].splice(0, 1)[0];
			queues1[to].unshift(crate);
		}

		// --- Puzzle 2 ---
		for (let i = count - 1; i >= 0; i--) {
			let crate = queues2[from].splice(i, 1)[0];
			queues2[to].unshift(crate);
		}
	}

});
await new Promise(res => reader.once("close", res));

console.log("Part 1. Crates on the top of each stack: ", queues1.map(q => q[0]).join(""));
console.log("Part 2. Crates on the top of each stack: ", queues2.map(q => q[0]).join(""));

