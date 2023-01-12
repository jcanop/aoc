import fs from "node:fs";
import readline from "node:readline";
import { from, to } from "./snafu.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Variables ---
let total = 0;

// --- Parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	total += from(line);
});
await new Promise(res => reader.once("close", res));
const snafu = to(total);
console.log("SNAFU number to supply to Bob's console:", snafu);
