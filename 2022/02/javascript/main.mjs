import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

const RESULT_WIN  = { id: "W", points: 6 };
const RESULT_DRAW = { id: "D", points: 3 };
const RESULT_LOSE = { id: "L", points: 0 };
const RESULTS = { X: RESULT_LOSE, Y: RESULT_DRAW, Z: RESULT_WIN };

const SHAPE_ROCK     = { id: "R", points: 1 };
const SHAPE_PAPER    = { id: "P", points: 2 };
const SHAPE_SCISSORS = { id: "S", points: 3 };
const SHAPES = {
	A: SHAPE_ROCK, B: SHAPE_PAPER, C: SHAPE_SCISSORS,
	X: SHAPE_ROCK, Y: SHAPE_PAPER, Z: SHAPE_SCISSORS,
};

// This function returns the shape to win, lose or draw against this shape.
function shapeTo(shape, result) {
	if (result === RESULT_WIN) {
		if (shape === SHAPE_ROCK) return SHAPE_PAPER;
		if (shape === SHAPE_PAPER) return SHAPE_SCISSORS;
		if (shape === SHAPE_SCISSORS) return SHAPE_ROCK;
	} else if (result === RESULT_LOSE) {
		if (shape === SHAPE_ROCK) return SHAPE_SCISSORS;
		if (shape === SHAPE_PAPER) return SHAPE_ROCK;
		if (shape === SHAPE_SCISSORS) return SHAPE_PAPER;
	} else if (result === RESULT_DRAW) {
		return shape;
	}
	return undefined;
}

// This function matches two shapes and returns a result.
function doMatch(me, op) {
	if (me === op) return RESULT_DRAW;
	if (( me === SHAPE_ROCK && op === SHAPE_SCISSORS) ||
		( me === SHAPE_PAPER && op === SHAPE_ROCK) ||
		( me === SHAPE_SCISSORS && op === SHAPE_PAPER)) return RESULT_WIN;
	return RESULT_LOSE;
}

// This function plays the game against this shape.
function play(me, op) {
	return doMatch(me, op).points + me.points;
}

// --- Parse the input file ---
let totals = [0 , 0];
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	// --- Puzzle 1 ---
	const op = SHAPES[line.charAt(0)];
	let me = SHAPES[line.charAt(2)];
	totals[0] += play(me, op);

	// --- Puzzle 2 ---
	const rs = RESULTS[line.charAt(2)];
	me = shapeTo(op, rs);
	totals[1] += play(me, op);
});
await new Promise(res => reader.once("close", res));
console.log("Part 1. Total score: ", totals[0].toLocaleString());
console.log("Part 2. Total score: ", totals[1].toLocaleString());
