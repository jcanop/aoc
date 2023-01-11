import fs from "node:fs";
import Grove from "./grove.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Read and parse the input file ---
const text = fs.readFileSync(INPUT_FILE, "utf8");
let grove = new Grove(text);
while (grove.sim()) {
	if (grove.round === 10) {
		let total = grove.emptyCount();
		console.log("1. Empty ground tiles:", total.toLocaleString());
	}
}
console.log("2. First round with no Elf moves", grove.round.toLocaleString());
