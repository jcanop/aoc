import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const REGEX = /^.+ can fly (\d+) km\/s for (\d+) seconds, but then must rest for (\d+) seconds\.$/;
const TIME = 2503;

// --- Functions ---
function reindeer(speed, fly, rest) {
	let r = {};
	r.speed = speed;
	r.fly = fly;
	r.rest = rest;
	r.distance = 0;
	r.flyCount = 0;
	r.restCount = 0;
	r.points = 0;
	r.tick = function() {
		if (this.flyCount > 0) {
			this.flyCount--;
			this.distance += this.speed;
		} else if (this.restCount > 0) {
			this.restCount--;
		} else {
			this.flyCount = this.fly - 1;
			this.restCount = this.rest;
			this.distance += this.speed;
		}
	};
	return r;
}

// --- Variables ---
let ls = [];

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	let cap = line.match(REGEX);
	let s = Number(cap[1]);
	let f = Number(cap[2]);
	let r = Number(cap[3]);
	ls.push(reindeer(s, f, r));
});
await new Promise(res => reader.once("close", res));

// --- Simulate race ---
for (let i = 0; i < TIME; i++) {
	ls.forEach(r => r.tick());
	let max = Math.max.apply(null, ls.map(r => r.distance));
	ls.forEach(r => { if (r.distance === max) r.points++; });
}

// --- Puzzle 1 ---
let max = Math.max.apply(null, ls.map(r => r.distance));
console.log("1. Winning reindeer distance: ", max.toLocaleString());

// --- Puzzle 2 ---
max = Math.max.apply(null, ls.map(r => r.points));
console.log("2. Winning reindeer points: ", max.toLocaleString());
