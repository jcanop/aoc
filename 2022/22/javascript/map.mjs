import  { EAST, LEFT, RIGHT, EMPTY, OPEN } from "./lib.mjs";

function Map(text) {
	this.grid = [];
	let max = 0;
	for (const line of text.split("\n")) {
		const chars = line.split("");
		max = Math.max(max, chars.length);
		this.grid.push(chars);
	}
	for (let chars of this.grid) {
		for (let i = chars.length; i < max; i++) chars.push(EMPTY);
	}
	this.height = this.grid.length;
	this.width  = max;
	let start = findStart(this.grid, this.width, this.height);
	this.x = start.x;
	this.y = start.y;
	this.d = EAST;

	this.path = function(path, move) {
		for (const cap of path.matchAll(/(\d+|R|L)/g)) {
			switch(cap[1]) {
				case LEFT:  this.d = (this.d === 0) ? 3 : this.d - 1; break;
				case RIGHT: this.d = (this.d === 3) ? 0 : this.d + 1; break;
				default: move(this, Number(cap[1]));
			}
		}
	};

	this.getPassword = function() {
		return 1_000 * (this.y + 1) + 4 * (this.x + 1) + this.d;
	};
}

function findStart(grid, width, height) {
	for (let y = 0; y < height; y++)
		for (let x = 0; x < width; x++)
			if (grid[y][x] === OPEN) return { x, y };
	throw "Start point not found!";
}

export default Map;
