import fs from "node:fs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Functions ---
function CPU() {
	let cpu = { a: 0, b: 0 };
	cpu.set = function(reg, value) {
		switch (reg) {
			case "a": this.a = value; break;
			case "b": this.b = value; break;
			default: throw "Illegal registry: " + reg;
		}
	};
	cpu.get = function(reg) {
		switch (reg) {
			case "a": return this.a;
			case "b": return this.b;
			default: throw "Illegal registry: " + reg;
		}
	};
	cpu.exec = function(code) {
		let i = 0;
		while (i < code.length) {
			let s = code[i].split(" ");
			s[1] = s[1].replace(",", "");
			switch (s[0]) {
				case "hlf": this.set(s[1], this.get(s[1]) / 2); i++; break;
				case "tpl": this.set(s[1], this.get(s[1]) * 3); i++; break;
				case "inc": this.set(s[1], this.get(s[1]) + 1); i++; break;
				case "jmp": i += Number(s[1]); break;
				case "jie": (this.get(s[1]) % 2 === 0) ? i += Number(s[2]) : i++; break;
				case "jio": (this.get(s[1]) === 1) ? i += Number(s[2]) : i++; break;
				default: throw "Illegal operation: " + s[0];
			}
		}
	};
	return cpu;
}

// --- Parse the input file ---
const input = fs.readFileSync(INPUT_FILE, 'utf8').trim();
const code = input.split("\n");

// --- Puzzle 1 ---
let cpu = CPU();
cpu.exec(code);
console.log("1. Register b: ", cpu.b.toLocaleString());

// --- Puzzle 2 ---
cpu = CPU();
cpu.a = 1;
cpu.exec(code);
console.log("2. Register b: ", cpu.b.toLocaleString());
