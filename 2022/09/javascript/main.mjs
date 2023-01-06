import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Creates a new Rope object ---
function create_rope(size) {
	let list = [];
	for (let i = 0; i < size; i++) list.push({ x: 0, y: 0 });
	let rope = {
		list,
		move: function(direction) {
			let head = this.list[0]
			switch (direction) {
				case 'U': head.y--; break;
				case 'D': head.y++; break;
				case 'L': head.x--; break;
				case 'R': head.x++; break;
				default: throw "Unreachable code";
			}
			for (let i = 1; i < this.list.length; i++) {
				let c = this.list[i - 1];
				let n = this.list[i];
				let dx = c.x - n.x;
				let dy = c.y - n.y;
				if (Math.abs(dx) < 2 && Math.abs(dy) < 2) return;
				n.x += Math.sign(dx);
				n.y += Math.sign(dy);
			}
		},
		tail: function() {
			let p = this.list[this.list.length - 1];
			return p.x + "," + p.y;
		}
	};
	return rope;
}

// --- Variables ---
var rope1 = create_rope(2);
var rope2 = create_rope(10);
var set1 = new Set();
var set2 = new Set();

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	let direction = line.charAt(0);
	let count = Number(line.substring(1));
	for (let i = 0; i < count; i++) {
		rope1.move(direction);
		rope2.move(direction);
		set1.add(rope1.tail());
		set2.add(rope2.tail());
	}
});
await new Promise(res => reader.once("close", res));

// --- Prints results ---
console.log("1. Positions visited at least once:", set1.size.toLocaleString());
console.log("2. Positions visited at least once:", set2.size.toLocaleString());
