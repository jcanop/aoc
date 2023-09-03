import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const STEPS = 100;
const ON = '#';
const OFF = '.';

// --- Functions ----
function countOnNeighbors(grid, x, y) {
	let c = 0;
	for (let j = -1; j < 2; j++) {
		if (j === -1 && y === 0) continue;
		if (j === 1 && y === grid.length -1) continue;
		const ny = y + j;
		for (let i = -1; i < 2; i++) {
			if (i === 0 && j === 0) continue;
			if (i === -1 && x === 0) continue;
			if (i === 1 && x === grid[0].length - 1) continue;
			const nx = x + i;
			if (grid[ny][nx] === ON) c++;
		}
	}
	return c;
}

function animation(data, puzzle) {
	// --- Init the two grids ---
	let grid1 = [];
	let grid2 = [];
	for (const row of data) {
		grid1.push([...row]);
		grid2.push([...row]);
	}
	const xLen = data[0].length;
	const yLen = data.length;

	// --- Run the animation ---
	let grid = grid1;
	let buffer = grid2;
	if (puzzle === 2) {
		grid[0][0] = ON;
		grid[0][xLen - 1] = ON;
		grid[yLen - 1][0] = ON;
		grid[yLen - 1][xLen - 1] = ON;
	}
	for (let step = 0; step < STEPS; step++) {
		for (let y = 0; y < yLen; y++) {
			for (let x = 0; x < xLen; x++) {
				if (puzzle === 2) {
					if ((x === 0 && y === 0) ||
						(x === 0 && y === yLen - 1) ||
						(x === xLen - 1 && y === 0) ||
						(x === xLen - 1 && y == yLen - 1)) {
						buffer[y][x] = ON;
						continue;
					}
				}
				let c = countOnNeighbors(grid, x, y);
				if (grid[y][x] === ON) {
					buffer[y][x] = (c === 2 || c === 3) ? ON : OFF;
				} else {
					buffer[y][x] = (c === 3) ? ON : OFF;
				}
			}
		}
		if (step % 2 === 0) {
			grid = grid2;
			buffer = grid1;
		} else {
			grid = grid1;
			buffer = grid2;
		}
	}

	// --- Return lights on ---
	return grid
		.map(row => row.map(x => x === ON ? 1 : 0).reduce((acc, x) => acc + x, 0))
		.reduce((acc, x) => acc + x, 0);
}

// --- Variables ---
let data = [];

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	data.push(Array.from(line));
});
await new Promise(res => reader.once("close", res));

// --- Puzzle 1 ---
let c = animation(data, 1);
console.log("1. Number of lights on: ", c.toLocaleString());

// --- Puzzle 2 ---
c = animation(data, 2);
console.log("2. Number of lights on: ", c.toLocaleString());
