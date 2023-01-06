import fs from "node:fs";
import readline from "node:readline";
import createTokenizer from "./tokenizer.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

function arrayItem(str) {
	if (!str.startsWith('[')) {
		str = "[" + str + "]";
	}
	let result = [];
	let tokenizer = createTokenizer(str);
	let t = null;
	while ((t = tokenizer.next()) != null) {
		result.push(t);
	}
	return result;
}

function compare(array1, array2) {
	const len = Math.max(array1.length, array2.length);
	for (let i = 0; i < len; i++) {
		if ( i === array1.length) return -1;
		if ( i === array2.length) return 1;
		const a = array1[i];
		const b = array2[i];
		if (!a.startsWith('[') && !b.startsWith('[')) {
			const va = Number(a);
			const vb = Number(b);
			const r = va - vb;
			if (r != 0) return r;
		} else {
			const va = arrayItem(a);
			const vb = arrayItem(b);
			const r = compare(va, vb);
			if (r != 0) return r;
		}
	}
	return 0;
}

// --- Variables ---
let list = [];
let a = null;
let b = null;
let index = 0;
let total = 0;

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	const id = Math.floor(index / 3) + 1;
	switch (index++ % 3) {
		case 0: a = arrayItem(line); list.push(a); break;
		case 1: b = arrayItem(line); list.push(b);
			if (compare(a, b) < 0) total += id;
			break;
	}
});
await new Promise(res => reader.once("close", res));

// --- Puzzle 1 --
console.log("1. The sum of the indices of the right order pairs:", total.toLocaleString());

// --- Puzzle 2 ---
const divider1 = arrayItem("[[2]]");
const divider2 = arrayItem("[[6]]");
list.push(divider1);
list.push(divider2);
list.sort(compare);
const index1 = list.indexOf(divider1) + 1;
const index2 = list.indexOf(divider2) + 1;
total = index1 * index2;
console.log("2. The decoder key:", total.toLocaleString());
