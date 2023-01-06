import fs from "node:fs";
import readline from "node:readline";
import { createMonkey, play } from "./monkey.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const REGEX = /Monkey (\d+):\s*Starting items: ([\d, ]+)\s*Operation: new = old ([\*\+]) (\d+|old)\s*Test: divisible by (\d+)\s*If true: throw to monkey (\d+)\s*If false: throw to monkey (\d+)/g

// --- Read and parse the input file ---
let group1 = [];
let group2 = [];
const text = fs.readFileSync(INPUT_FILE, "utf8");
for (const cap of text.matchAll(REGEX)) {
	group1.push(createMonkey(cap));
	group2.push(createMonkey(cap));
}

// --- Puzzle 1 ---
for (let i = 0; i < 20; i++) for (let m = 0; m < group1.length; m++) play(m, group1, 1);
let total = group1.map(m => m.inspects)
	.sort((a, b) => b - a)
	.splice(0, 2)
	.reduce((acc, val) => acc * val, 1);
console.log("1. Level of monkey business:", total.toLocaleString());

// --- Puzzle 2 ---
for (let i = 0; i < 10000; i++) for (let m = 0; m < group2.length; m++) play(m, group2, 2);
total = group2.map(m => m.inspects)
	.sort((a, b) => b - a)
	.splice(0, 2)
	.reduce((acc, val) => acc * val, 1);
console.log("2. Level of monkey business:", total.toLocaleString());
