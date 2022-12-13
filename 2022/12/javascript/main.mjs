import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// Loads the map
async function load_map(filename) {
	let grid = [];
	let start = {};
	let end = [];
	const stream = fs.createReadStream(filename);
	const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
	reader.on("line", line => {
		grid.push(Array.from(line));
	});
	await new Promise(res => reader.once("close", res));

	for (let y = 0; y < grid.length; y++) {
		for (let x = 0; x < grid[0].length; x++) {
			if (grid[y][x] === "S") { start = { x, y }; grid[y][x] = 'a'; }
			if (grid[y][x] === "E") { end = { x, y }; grid[y][x] = 'z'; }
		}
	}

	let map = { grid, start, end };
	return map;
}

// --- Read and parse the input file ---
let map = await load_map(INPUT_FILE);

// --- Search the smallest path ---
map.findPath = function() {
	const h = this.grid.length;
	const w = this.grid[0].length;
	let a = [];
	for (let y = 0; y < h; y++) {
		let b = [];
		for (let x = 0; x < w; x++) {
			b.push(0);
		}
		a.push(b);
	}
	let x = this.start.x;
	let y = this.start.y;
	a[y][x] = 1;
	let list = [{ x, y }];
	while (list.length > 0) {
		const p = list.splice(0, 1)[0];
		x = p.x;
		y = p.y;
		if (x > 0     && a[y][x - 1] === 0 && this.grid[y][x - 1].charCodeAt(0) <= this.grid[y][x].charCodeAt(0) + 1) { a[y][x - 1] = a[y][x] + 1; list.push({ x: x - 1, y }); }
		if (y > 0     && a[y - 1][x] === 0 && this.grid[y - 1][x].charCodeAt(0) <= this.grid[y][x].charCodeAt(0) + 1) { a[y - 1][x] = a[y][x] + 1; list.push({ x, y: y - 1 }); }
		if (x < w - 1 && a[y][x + 1] === 0 && this.grid[y][x + 1].charCodeAt(0) <= this.grid[y][x].charCodeAt(0) + 1) { a[y][x + 1] = a[y][x] + 1; list.push({ x: x + 1, y }); }
		if (y < h - 1 && a[y + 1][x] === 0 && this.grid[y + 1][x].charCodeAt(0) <= this.grid[y][x].charCodeAt(0) + 1) { a[y + 1][x] = a[y][x] + 1; list.push({ x, y: y + 1 }); }
	}

	x = this.end.x;
	y = this.end.y;
	let c = a[y][x];
	list = [];
	while (c-- > 1) {
		if (x > 0 && a[y][x - 1] == c) x--;
		else if (y > 0 && a[y - 1][x] == c) y--;
		else if (x < w - 1 && a[y][x + 1] == c) x++;
		else if (y < h - 1 && a[y + 1][x] == c) y++;
		list.push({ x, y });
	}
	return list;
};

// --- Searches all the paths from the lowest elevation points ---
map.findAllPaths = function() {
		const h = this.grid.length;
		const w = this.grid[0].length;
		let list = [];
		for (let y = 0; y < h; y++) {
			for (let x = 0; x < w; x++) {
				if (this.grid[y][x] == 'a') {
					this.start = { x, y };
					let path = this.findPath();
					if (path.length > 0) list.push(path);
				}
			}
		}
		return list;
};

// --- Puzzle 1 ---
const count = map.findPath().length;
console.log("1. Fewest steps: ", count);

// --- Puzzle 2 ---
const min = Math.min(...map.findAllPaths().map(x => x.length));
console.log("2. Shortest path: ", min);
