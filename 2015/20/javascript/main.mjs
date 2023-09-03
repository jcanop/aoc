import fs from "node:fs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Functions ---
function divisorsSum(n) {
	let sum = 1;
	for (let i = 2; i < n; i++) {
		i * i > n ? i = n : 0;
		let b = 0;
		while (n % i < 1) {
			let c = sum * i;
			sum += c - b;
			b = c;
			n /= i;
		}
	}
	return sum;
}

function divisors(n) {
	const a = n > 1 ? [1, n] : [1];
	for (let i = 2; i <= Math.sqrt(n); i++) {
		if (n % i === 0) {
			a.push(i);
			if (i ** 2 !== n) a.push(n / i);
		}
	}
	return a;
}

// --- Parse the input file ---
const input = Number(fs.readFileSync(INPUT_FILE, 'utf8'));

// --- Puzzle 1 ---
let house = 0;
let presents = 0;
while (presents < input) {
	house++;
	presents = 10 * divisorsSum(house);
}
console.log("1. House number: ", house.toLocaleString());

// --- Puzzle 2 ---
house = 0;
presents = 0;
while (presents < input) {
	house++;
	presents = 11 * divisors(house).filter(x => x * 50 >= house).reduce((acc, x) => acc + x);
}
console.log("2. House number: ", house.toLocaleString());
