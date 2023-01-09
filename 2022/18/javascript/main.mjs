import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Derives a point from another ---
function from(p, dx, dy, dz) {
	const x = p.x + dx;
	const y = p.y + dy;
	const z = p.z + dz;
	const id = x + "," + y + "," + z;
	return { id, x, y, z };
}

// --- Calculates the surface ---
function calculate(data, list) {
	let total = 0;
	for (const [x, y, z] of list) {
		total += 6;
		if (x > 0 && data[x - 1][y][z] === 1) total--;
		if (y > 0 && data[x][y - 1][z] === 1) total--;
		if (z > 0 && data[x][y][z - 1] === 1) total--;
		if (x < data.length - 1 && data[x + 1][y][z] === 1) total--;
		if (y < data[0].length - 1 && data[x][y + 1][z] === 1) total--;
		if (z < data[0][0].length - 1 && data[x][y][z + 1] === 1) total--;
	}
	return total;
}

// --- Searches for a path to the edge of the model ---
function dfs(root, data, closed) {
	let list = [];
	let visited = new Set();
	list.push(root);
	visited.add(root.x + "," + root.y + "," + root.z);

	while (list.length > 0) {
		let p = list.shift();
		if (p.x === 0 || p.x === data.length - 1 ||
			p.y === 0 || p.y === data[0].length - 1 ||
			p.z === 0 || p.z === data[0][0].length - 1) return true;

		const ns = [
			from(p, -1,  0,  0), from(p, 1, 0, 0),
			from(p,  0, -1,  0), from(p, 0, 1, 0),
			from(p,  0,  0, -1), from(p, 0, 0, 1)
		];
		for (const n of ns) {
			const id = n.x + "," + n.y + "," + n.z;
			if (data[n.x][n.y][n.z] === 0 && !visited.has(id)) {
				list.push(n);
				visited.add(id);
			}
		}
	}

	visited.forEach(p => closed.add(p));
	return false;
}

// --- Variables ---
let list = [];
let max = [0, 0, 0];

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	let tokens = line.split(",");
	const x = Number(tokens[0]);
	const y = Number(tokens[1]);
	const z = Number(tokens[2]);
	list.push([x, y, z]);
	max[0] = Math.max(x, max[0]);
	max[1] = Math.max(y, max[1]);
	max[2] = Math.max(z, max[2]);
});
await new Promise(res => reader.once("close", res));

// --- Initialice the 3D map ---
let data = Array(max[0] + 1).fill(0);
for (let i = 0; i <= max[0]; i++) {
	data[i] = Array(max[1] + 1).fill(0);
	for (let j = 0; j <= max[1]; j++) {
		data[i][j] = Array(max[2] + 1).fill(0);
	}
}
for (const [x, y, z] of list) {
	data[x][y][z] = 1;
}

// --- Puzzle 1 ---
let total = calculate(data, list);
console.log("1. Total surface:", total.toLocaleString());

// --- Puzzle 2 ---
let closed = new Set();
for (let x = 1; x < data.length - 1; x++) {
	for (let y = 1; y < data[0].length - 1; y++) {
		for (let z = 1; z < data[0][0].length - 1; z++) {
			if (data[x][y][z] === 1) continue;
			const id = x + "," + y + "," + z;
			if (!closed.has(id)) {
				const p = { x, y, z };
				dfs(p, data, closed);
			}
		}
	}
}
for (const p of closed) {
	const tokens = p.split(",");
	const x = Number(tokens[0]);
	const y = Number(tokens[1]);
	const z = Number(tokens[2]);
	data[x][y][z] = 1;
}
total = calculate(data, list);
console.log("2. Adjusted total surface:", total.toLocaleString());
