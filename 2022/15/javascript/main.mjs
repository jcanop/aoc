import fs from "node:fs";
import createMap from "./map.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const PUZZLE1_LINE = 2_000_000;
const PUZZLE2_MIN = 0;
const PUZZLE2_MAX = 4_000_000;
const REGEX = /Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/g;

// --- Helper functions ---
function inLimit(x, y) {
	return x >= PUZZLE2_MIN && x <= PUZZLE2_MAX &&
		y >= PUZZLE2_MIN && y <= PUZZLE2_MAX;
}

function calculateFreq(x, y) {
	return x * PUZZLE2_MAX + y;
}

// --- Read and parse the input file ---
let map = createMap();
const text = fs.readFileSync(INPUT_FILE, "utf8");
for (const cap of text.matchAll(REGEX)) {
	const x = cap[1];
	const y = cap[2];
	const bx = cap[3];
	const by = cap[4];
	map.addSensor(x, y, bx, by);
}

// --- Search max and min ---
let minx = Number.MAX_VALUE;
let maxx = 0;
let range = 0;
for (let s of Object.values(map.sensors)) {
	minx = Math.min(minx, s.x);
	maxx = Math.max(maxx, s.x);
	range = Math.max(range, s.range);
}
minx -= range;
maxx += range;

// --- Puzzle 1 ---
let total = 0;
for (let x = minx; x <= maxx; x++) {
	if (map.isInSensorRange(x, PUZZLE1_LINE) && map.isEmpty(x, PUZZLE1_LINE)) total++;
}
console.log("1. Position that cannot contain a beacon:", total.toLocaleString());

// --- Puzzle 2 ---
let freq = 0;
for (const s of Object.values(map.sensors)) {
	for (let i = 0; i <= s.range; i++) {
		// North -> East
		let x = s.x + i;
		let y = s.y - s.range + i - 1;
		if (inLimit(x, y) && !map.isInSensorRange(x, y)) { freq = calculateFreq(x, y); break; }

		// East -> South
		x = s.x + s.range - i + 1;
		y = s.y + i;
		if (inLimit(x, y) && !map.isInSensorRange(x, y)) { freq = calculateFreq(x, y); break; }

		// South -> West
		x = s.x - i;
		y = s.y + s.range - i  + 1;
		if (inLimit(x, y) && !map.isInSensorRange(x, y)) { freq = calculateFreq(x, y); break; }

		// West -> North
		x = s.x - s.range + i - 1;
		y = s.y - i;
		if (inLimit(x, y) && !map.isInSensorRange(x, y)) { freq = calculateFreq(x, y); break; }
	}
	if (freq > 0) break;
}
console.log("2. Frequency:", freq.toLocaleString());
