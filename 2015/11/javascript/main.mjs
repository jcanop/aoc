import fs from "node:fs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Functions ---
function sanitize(a) {
	for (let i = 0; i < a.length; i++) {
		if (a[i] === 'i' || a[i] === 'o' || a[i] === 'l') {
			a[i]++;
			for (let j = i + 1; j < a.length; j++) {
				a[j] = 'a';
			}
			break;
		}
	}
}

function next(a) {
	for (let i = a.length - 1; i >= 0; i--) {
		a[i] = String.fromCharCode(a[i].charCodeAt(0) + 1);
		if (a[i] === 'i' || a[i] === 'o' || a[i] === 'l') a[i] = String.fromCharCode(a[i].charCodeAt(0) + 1);
		if (a[i] > 'z') a[i] = 'a';
		else break;
	}
}

function rule_1(a) {
	for (let i = 0; i < a.length - 2; i++) {
		if (a[i +  1].charCodeAt(0) === a[i].charCodeAt(0) + 1 &&
			a[i + 2].charCodeAt(0) === a[i].charCodeAt(0) + 2) return true;
	}
	return false;
}

function rule_2(a) {
	let c = ' ';
	let n = 0;
	for (let i = 0; i < a.length - 1; i++) {
		if (a[i] === a[i + 1] && c !== a[i]) {
			n++;
			c = a[i];
		}
		if (n === 2) return true;
	}
	return false;
}

// --- Parse the input file ---
const text = fs.readFileSync(INPUT_FILE, 'utf8').trim();
let pass = Array.from(text);

// --- Search next pasword ---
let count = 1;
sanitize(pass);
while (true) {
	next(pass);
	if (rule_1(pass) && rule_2(pass)) {
		console.log(count + ". Next password: ", pass.join(""));
		count++;
		if (count > 2) break;
	}
}
