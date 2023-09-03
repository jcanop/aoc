import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

function solve(op, wires, cache) {
	if (!isNaN(op)) return Number(op);
	if (cache[op] !== undefined) return cache[op];

	function _solve(o) { return solve(o, wires, cache); }

	let r;
	const ls = wires[op].split(" ");
	if (ls.includes("AND"))         r = _solve(ls[0]) & _solve(ls[2]);
	else if (ls.includes("OR"))     r = _solve(ls[0]) | _solve(ls[2]);
	else if (ls.includes("LSHIFT")) r = _solve(ls[0]) << _solve(ls[2]);
	else if (ls.includes("RSHIFT")) r = _solve(ls[0]) >> _solve(ls[2]);
	else if (ls.includes("NOT"))    r = ~ _solve(ls[1]);
	else r = _solve(ls[0]);
	cache[op] = r;
	return r;
}

// --- Variables ---
let wires = {};
let cache = {};

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	const ls = line.split(" -> ");
	wires[ls[1].trim()] = ls[0].trim();
});
await new Promise(res => reader.once("close", res));

// --- Puzzle 1 ---
let r = solve("a", wires, cache);
console.log("1. Wire a: ", r.toLocaleString());

// --- Puzzle 2 ---
wires["b"] = r.toString();
cache = {};
r = solve("a", wires, cache);
console.log("2. Wire a: ", r.toLocaleString());
