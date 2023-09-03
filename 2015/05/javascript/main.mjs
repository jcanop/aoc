import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const LIST_1 = ['a', 'e', 'i', 'o', 'u'];
const LIST_2 = ["ab", "cd", "pq", "xy"];

// --- Rules ---
function rule_1_1(s) {
	let count = 0;
	for (const c of Array.from(s)) {
		if (LIST_1.includes(c)) count++;
		if (count == 3) return true;
	}
	return false;
}

function rule_1_2(s) {
	const a = Array.from(s);
	for (let i = 0; i < a.length - 1; i++) {
		if (a[i] == a[i + 1]) return true;
	}
	return false;
}

function rule_1_3(s) {
	for (const c of LIST_2) {
		if (s.includes(c)) return false;
	}
	return true;
}

function rule_2_1(s) {
	const a = Array.from(s);
	for (let i = 0; i < a.length - 2; i++) {
		for (let j = i + 2; j < a.length - 1; j++) {
			if (a[i] == a[j] && a[i + 1] == a[j + 1]) return true;
		}
	}
	return false;
}

function rule_2_2(s) {
	const a = Array.from(s);
	for (let i = 0; i < a.length - 2; i++) {
		if (a[i] == a[i + 2]) return true;
	}
	return false;
}

// --- Variables ---
let count1 = 0;
let count2 = 0;

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	if (rule_1_1(line) && rule_1_2(line) && rule_1_3(line)) count1++;
	if (rule_2_1(line) && rule_2_2(line)) count2++;
});
await new Promise(res => reader.once("close", res));

// --- Puzzle 1 ---
console.log("1. Nice Strings: ", count1.toLocaleString());

// --- Puzzle 2 ---
console.log("2. Nice Strings: ", count2.toLocaleString());
