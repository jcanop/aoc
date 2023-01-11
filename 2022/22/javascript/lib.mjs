// --- Constants ---
const EMPTY  = ' ';
const OPEN   = '.';
const WALL   = '#';
const RIGHT  = 'R';
const LEFT   = 'L';
const EAST   = 0;
const SOUTH  = 1;
const WEST   = 2;
const NORTH  = 3;

// --- Move functions ---
function moveGrid(map, steps) {
	let x = map.x;
	let y = map.y;

	while (steps > 0) {
		switch(map.d) {
			case NORTH: y = (y === 0) ? map.height - 1 : y - 1; break;
			case SOUTH: y = (y === map.height - 1) ? 0 : y + 1; break;
			case WEST:  x = (x === 0) ? map.width - 1  : x - 1; break;
			case EAST:  x = (x === map.width - 1)  ? 0 : x + 1; break;
			default: throw "Unreachable!";
		}
		if (map.grid[y][x] === WALL) break;
		if (map.grid[y][x] === OPEN) {
			steps--;
			map.x = x;
			map.y = y;
		}
	}
}

function moveCube(map, steps) {
	while (steps > 0) {
		let x = map.x;
		let y = map.y;
		let d = map.d;

		switch (map.d) {
			case NORTH: y--; break;
			case SOUTH: y++; break;
			case WEST:  x--; break;
			case EAST:  x++; break;
			default: throw "Unreachable!";
		}

		if (x < 0 || x >= map.width || y < 0 || y >= map.height || map.grid[y][x] === EMPTY) {
			let r = Math.floor(map.y / 50);
			let c = Math.floor(map.x / 50);

			if      (r == 0 && c == 1 && d == NORTH) { r = 3; c = 0; d = EAST;  }
			else if (r == 0 && c == 1 && d == WEST)  { r = 2; c = 0; d = EAST;  }
			else if (r == 0 && c == 2 && d == NORTH) { r = 3; c = 0; d = NORTH; }
			else if (r == 0 && c == 2 && d == EAST)  { r = 2; c = 1; d = WEST;  }
			else if (r == 0 && c == 2 && d == SOUTH) { r = 1; c = 1; d = WEST;  }
			else if (r == 1 && c == 1 && d == EAST)  { r = 0; c = 2; d = NORTH; }
			else if (r == 1 && c == 1 && d == WEST)  { r = 2; c = 0; d = SOUTH; }
			else if (r == 2 && c == 0 && d == NORTH) { r = 1; c = 1; d = EAST;  }
			else if (r == 2 && c == 0 && d == WEST)  { r = 0; c = 1; d = EAST;  }
			else if (r == 2 && c == 1 && d == EAST)  { r = 0; c = 2; d = WEST;  }
			else if (r == 2 && c == 1 && d == SOUTH) { r = 3; c = 0; d = WEST;  }
			else if (r == 3 && c == 0 && d == EAST)  { r = 2; c = 1; d = NORTH; }
			else if (r == 3 && c == 0 && d == SOUTH) { r = 0; c = 2; d = SOUTH; }
			else if (r == 3 && c == 0 && d == WEST)  { r = 0; c = 1; d = SOUTH; }
			else throw "Unreachable!";

			let ci = map.x % 50;
			let ri = map.y % 50;

			let i = -1;
			switch (map.d) {
				case NORTH: i = ci; break;
				case SOUTH: i = 49 - ci; break;
				case WEST:  i = 49 - ri; break;
				case EAST:  i = ri; break;
				default: throw "Unreachable!";
			}

			let rn = -1, cn = -1;
			switch (d) {
				case NORTH: rn = 49; cn = i; break;
				case SOUTH: rn = 0; cn = 49 - i; break;
				case WEST:  rn = 49 - i; cn = 49; break;
				case EAST:  rn = i; cn = 0; break;
				default: throw "Unreachable!";
			}

			x = 50 * c + cn;
			y = 50 * r + rn;
		}

		if (map.grid[y][x] === WALL) break;
		if (map.grid[y][x] === OPEN) {
			steps--;
			map.x = x;
			map.y = y;
			map.d = d;
		}
	}
}

export { EAST, LEFT, RIGHT, EMPTY, OPEN, moveGrid, moveCube };
