import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

let CPU = {
	program: [],
	registry: 1,
	command: null,
	tick: function() {
		if (this.program.length === 0) return false;
		if (this.command === null) {
			let cmd = this.program.shift();
			if (cmd === "noop") return true;
			if (cmd.startsWith("addx")) {
				this.command = cmd;
				return true;
			}
			throw "Unsupported command: " + cmd;
		}
		this.registry += Number(this.command.substring(5));
		this.command = null;
		return this.program.length > 0;
	}
}

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	CPU.program.push(line);
});
await new Promise(res => reader.once("close", res));

let cycle = 1;
let mark = 20;
let total = 0;
let buffer = "";
do {
	// --- Puzzle 1 ---
	if (cycle === mark) {
		total += CPU.registry * mark;
		mark += 40;
	}

	// --- Puzzle 2 ---
	let p = (cycle - 1) % 40;
	let c = (p >= CPU.registry - 1 && p <= CPU.registry + 1) ? "#" : " ";
	buffer += c;
	if (cycle % 40 === 0) buffer += "\n";

	// --- Both ---
	cycle++;
} while (CPU.tick());

// --- Puzzle 1 ---
console.log("1. The sum of the signal strenghts is:", total.toLocaleString());

// --- Puzzle 2 ---
console.log("2. Image on the CRT");
console.log(buffer);
