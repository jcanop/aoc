import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const ORIGIN = "+";
const AIR = " ";
const ROCK = "#";
const SAND = "o";
const OFFSET = 2;
const ORIGIN_POINT = { x: 500, y: 0 };

// --- Loads a Map from the input file ---
async function loadMap(filename) {
	let list = [];
	let height = 0;

	// --- Parse the input file ---
	const stream = fs.createReadStream(filename);
	const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
	reader.on("line", line => {
		let ln = [];
		for (let token of line.split(/\s*->\s*/)) {
			const s = token.split(/\s*,\s*/);
			const x = Number(s[0]);
			const y = Number(s[1]);
			ln.push({ x, y });
			height = Math.max(y, height);
		}
		list.push(ln);
	});
	await new Promise(res => reader.once("close", res));

	let data = {};
	height += 3;
	for (let ln of list) {
		for (let i = 1; i < ln.length; i++) {
			const p1 = ln[i - 1];
			const p2 = ln[i];
			const x1 = Math.min(p1.x, p2.x);
			const x2 = Math.max(p1.x, p2.x);
			const y1 = Math.min(p1.y, p2.y);
			const y2 = Math.max(p1.y, p2.y);
			for (let y = y1; y <= y2; y++) {
				for (let x = x1; x <= x2; x++) {
					data[x + "-" + y] = ROCK;
				}
			}
		}
	}

	// --- Get the title at one position ---
	const get = function(x, y) {
		if (y === this.height - 1) return ROCK;
		const tile = this.data[x + "-" + y];
		if (tile === undefined) {
			if (x === ORIGIN_POINT.x && y === ORIGIN_POINT.y) return ORIGIN;
			return AIR;
		}
		return tile;
	}

	// --- Counts the SAND tiles ---
	const countSandTiles = function() {
		return Object.values(this.data).filter(x => x === SAND).reduce((a, v) => a + 1, 0);
	};

	return { data, height, get, countSandTiles };
}

// --- Runs the simulation ---
function sim(map, puzzle) {
	while (true) {
		let x = ORIGIN_POINT.x;
		let y = ORIGIN_POINT.y;
		while(true) {
			if (puzzle === 1 && y + 1 === map.height - 1)  return;
			if (map.get(x,     y + 1) === AIR) { y++;      continue; }
			if (map.get(x - 1, y + 1) === AIR) { y++; x--; continue; }
			if (map.get(x + 1, y + 1) === AIR) { y++; x++; continue; }
			map.data[x + "-" + y] = SAND;
			if (x === ORIGIN_POINT.x && y === ORIGIN_POINT.y) return;
			break;
		}
	}
}

export { loadMap, sim };
