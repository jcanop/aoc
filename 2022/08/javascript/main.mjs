import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// Checks if a tree is visible.
function isVisible(grid, x, y) {
	for (let i = y + 1; i < grid.length; i++) {
		if (grid[i][x] >= grid[y][x]) break;
		if (i === grid.length - 1) return true;
	}
	for (let i = y - 1; i >= 0; i--) {
		if (grid[i][x] >= grid[y][x]) break;
		if (i === 0) return true;
	}
	for (let i = x + 1; i < grid[0].length; i++) {
		if (grid[y][i] >= grid[y][x]) break;
		if (i === grid[0].length - 1) return true;
	}
	for (let i = x - 1; i >= 0; i--) {
		if (grid[y][i] >= grid[y][x]) return false;
		if (i === 0) return true;
	}
}

// Calculates the scenic score of a tree.
function scenicScore(grid, x, y) {
	let a = 1;
	for (let i = y + 1; i < grid.length - 1; i++) {
		if (grid[i][x] >= grid[y][x]) break;
		a++;
	}
	let b = 1;
	for (let i = y - 1; i > 0; i--) {
		if (grid[i][x] >= grid[y][x]) break;
		b++;
	}
	let c = 1;
	for (let i = x + 1; i < grid[0].length - 1; i++) {
		if (grid[y][i] >= grid[y][x]) break;
		c++;
	}
	let d = 1;
	for (let i = x - 1; i > 0; i--) {
		if (grid[y][i] >= grid[y][x]) break;
		d++;
	}
	return a * b * c * d;
}

// --- Read and parse the input file ---
let grid = [];
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	grid.push(Array.from(line));
});
await new Promise(res => reader.once("close", res));

// --- Puzzle 1 ---
let total = (grid.length + grid[0].length - 2) * 2;
for (let y = 1; y < grid.length - 1; y++)
	for (let x = 1; x < grid[0].length - 1; x++)
		total += isVisible(grid, x, y) ? 1 : 0;
console.log("1. Trees that are visible from aoutside the grid: ", total.toLocaleString());

// --- Puzzle 2 ---
let max = 0;
for (let y = 1; y < grid.length - 1; y++)
	for (let x = 1; x < grid[0].length - 1; x++) {
		const score = scenicScore(grid, x, y);
		if (score > max) max = score;
	}
console.log("2. The highest scenic score is: ", max.toLocaleString());
