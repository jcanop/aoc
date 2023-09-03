import fs from "node:fs";
import md5 from "../../../commons/javascript/md5.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Parse the input file ---
const secret = fs.readFileSync(INPUT_FILE, 'utf8').trim();

// --- Compute the hash ---
let found1 = false;
let found2 = false;
let count = 0;

while(true) {
	const text = secret + count;
	const hex = md5(text);

	// --- Puzzle 1 ---
	if (!found1 && hex.substring(0, 5) === "00000") {
		console.log("1. First hash with five zeros: ", count.toLocaleString());
		found1 = true;
	}

	// --- Puzzle 2 ---
	if (!found2 && hex.substring(0, 6) === "000000") {
		console.log("2. First hash with six zeros: ", count.toLocaleString());
		found2 = true;
	}

	if (found1 && found2) break;
	count++;
}
