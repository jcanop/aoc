import fs from "node:fs";
import { SHAPES, createChamber } from "./chamber.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const LIMIT_1 = 2022;
const LIMIT_2 = 1_000_000_000_000;

// --- Runs the simulations ---
function sim(flows, limit) {
	let count = 0;
	let rocks = 1;
	const gcd = SHAPES.length * flows.length; // Greates Common Divisor
	let chamber = createChamber();
	chamber.addShape(SHAPES[count % SHAPES.length]);

	// --- Variables for tracking a pattern ---
	let prevHeight = 0;
	let prevRocks = 0;
	let deltaHeight = 0;
	let deltaRocks = 0;
	let simulated = 0;

	// --- Iterate until the limit is reached ---
	while (true) {
		if (count > 0 && count % gcd === 0) {
			const height = chamber.getHeight();
			const dh = height - prevHeight;
			const dr = rocks - prevRocks;
			if (dh === deltaHeight && dr === deltaRocks) {
				const rate = Math.floor((limit - rocks) / dr);
				simulated = dh * rate + 1;
				rocks = limit - ((limit - rocks) % dr);
			}
			prevHeight = height;
			prevRocks = rocks;
			deltaHeight = dh;
			deltaRocks = dr;
		}

		// --- Normal simulation ---
		chamber.move(flows[count % flows.length]);
		if (!chamber.down()) {
			if (rocks > limit) {
				return chamber.getHeight() + simulated;
			}
			chamber.addShape(SHAPES[rocks % SHAPES.length]);
			rocks++;
		}
		count++;
	}
}

// --- Reads and parse the input file ---
const text = fs.readFileSync(INPUT_FILE, "utf8");
const flows = Array.from(text.trim());

// --- Puzzle 1 ---
let height = sim(flows, LIMIT_1);
console.log("1. Tower height:", height.toLocaleString());

// --- Puzzle 2 ---
height = sim(flows, LIMIT_2);
console.log("2. Tower height:", height.toLocaleString());
