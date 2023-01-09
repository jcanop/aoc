const SHAPES = [
	[{x: 0, y: 0}, {x: 1, y: 0}, {x: 2, y: 0}, {x: 3, y: 0}],
	[{x: 1, y: 0}, {x: 0, y: 1}, {x: 1, y: 1}, {x: 2, y: 1}, {x: 1, y: 2}],
	[{x: 2, y: 0}, {x: 2, y: 1}, {x: 0, y: 2}, {x: 1, y: 2}, {x: 2, y: 2}],
	[{x: 0, y: 0}, {x: 0, y: 1}, {x: 0, y: 2}, {x: 0, y: 3}],
	[{x: 0, y: 0}, {x: 1, y: 0}, {x: 0, y: 1}, {x: 1, y: 1}]
];

const WIDTH = 7;
const MARGIN_LEFT = 2;
const MARGIN_TOP = 3;
const AIR = '.';
const ROCK = '@';
const SAND = '#';
const LEFT = '<';
const RIGHT = '>';
const DOWN = 'v';

function createChamber() {
	let rows = [];
	let shapeLen = 0;

	const findFromTop = function(c) {
		for (let y = rows.length - 1; y >= 0; y--) {
			for (let x = 0; x < WIDTH; x++) {
				if (rows[y][x] == c) return y;
			}
		}
	};

	const getHeight = function() {
		return findFromTop(SAND) - 1;
	};

	const addShape = function(shape) {
		// --- Remove extra rows ---
		const top = findFromTop(SAND);
		const n =  rows.length - top - 1;
		rows.splice(top + 1, n);

		// --- Add rows ---
		shapeLen = shape[shape.length - 1].y + 1;
		for (let i = 0; i < MARGIN_TOP + shapeLen; i++) {
			const row = Array(WIDTH).fill(AIR);
			rows.push(row);
		}

		// --- Add shape ---
		const h = rows.length - 1;
		for (const p of shape) {
			rows[h - p.y][MARGIN_LEFT + p.x] = ROCK;
		}
	}

	const canMove = function(direction) {
		let i = findFromTop(ROCK);
		if (i == -1) throw "Shape not found!";
		if (direction === DOWN) {
			i -= shapeLen - 1;
			if (i === 0) return false;
			for (let y = i; y < i + shapeLen; y++) {
				for (let x = 0; x < WIDTH; x++) {
					if (rows[y][x] === ROCK && rows[y - 1][x] === SAND) return false;
				}
			}
		} else if (direction === LEFT) {
			for (let y = i; y > i - shapeLen; y--) {
				if (rows[y][0] === ROCK) return false;
				for (let x = 1; x < WIDTH; x++) {
					if (rows[y][x] === ROCK) {
						if (rows[y][x - 1] !== AIR) return false;
						break;
					}
				}
			}
		} else if (direction === RIGHT) {
			for (let y = i; y > i - shapeLen; y--) {
				if (rows[y][WIDTH - 1] === ROCK) return false;
				for (let x = WIDTH - 2; x >= 0; x--) {
					if (rows[y][x] === ROCK) {
						if (rows[y][x + 1] !== AIR) return false;
						break;
					}
				}
			}
		} else {
			throw "Usupported direction: " + direction;
		}
		return true;
	}

	const move = function(direction) {
		if (canMove(direction)) {
			if (direction === DOWN) {
				const i = findFromTop(ROCK) - (shapeLen - 1);
				for (let y = i; y < i + shapeLen; y++) {
					for (let x = 0; x < WIDTH; x++) {
						if (rows[y][x] === ROCK) {
							rows[y - 1][x] = rows[y][x];
							rows[y][x] = AIR;
						}
					}
				}
			} else if (direction === LEFT) {
				const i = findFromTop(ROCK);
				for (let y = i; y > i - shapeLen; y--) {
					for (let x = 0; x < WIDTH - 1; x++) {
						if (rows[y][x + 1] === ROCK) {
							rows[y][x] = rows[y][x + 1];
							rows[y][x + 1] = AIR;
						}
					}
				}
			} else if (direction === RIGHT) {
				const i = findFromTop(ROCK);
				for (let y = i; y > i - shapeLen; y--) {
					for (let x = WIDTH - 1; x > 0; x--) {
						if (rows[y][x - 1] === ROCK) {
							rows[y][x] = rows[y][x - 1];
							rows[y][x - 1] = AIR;
						}
					}
				}
			} else {
				throw "Usupported direction: " + direction;
			}
		} else if (direction === DOWN) {
			const i = findFromTop(ROCK) - (shapeLen - 1);
			for (let y = i; y < i + shapeLen; y++) {
				for (let x = 0; x < WIDTH; x++) {
					if (rows[y][x] === ROCK) rows[y][x] = SAND;
				}
			}
			return false;
		}
		return true;
	}

	const down = function() {
		return move(DOWN);
	}

	const print = function() {
		let buffer = "";
		for (let i = rows.length - 1; i >= 0; i--) {
			buffer += "|";
			for (const c of rows[i]) buffer += c;
			buffer += "|\n";
		}
		buffer += "+";
		for (let i = 0; i < WIDTH; i++) buffer += "-";
		buffer += "+\n";
		console.log(buffer);
	}

	return { rows, shapeLen, getHeight, addShape, down, move, print };
}

export { SHAPES, createChamber };
