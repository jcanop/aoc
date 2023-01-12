import Blizzard from "./blizzard.mjs";

function Map(text) {
	this.blizzard = new Blizzard();
	this.data = [];
	this.next = [];
	const lines = text.trim().split("\n");
	this.height = lines.length;
	this.width = lines[0].length;

	// --- Loads an input file and creats a map ---
	for (let i = 1; i < lines.length - 1; i++) {
		const line = lines[i];
		this.blizzard.add(line.substring(1, line.length - 1));
	}
	for (let y = 0; y < this.height; y++) {
		this.data.push(Array(this.width));
		this.next.push(Array(this.width));
	}

	// --- checks if you can move to a space at a time ---
	this.canMove = function(x, y, time) {
		if (x === 1 && y === 0) return true; // Start position
		if (x === this.width - 2 && y === this.height - 1) return true; // End position
		if (x < 1 || x >= this.width - 1 || y < 1 || y >= this.height - 1) return false; // Border
		return this.blizzard.isEmpty(x, y, time);
	};

	// --- Search the fastest path between two points ---
	this.search = function(start, end, time) {
		for (let y = 0; y < this.height; y++) {
			this.data[y].fill(' ');
			this.next[y].fill(' ');
		}
		this.data[start.y][start.x] = 'X';

		while (this.data[end.y][end.x] !== 'X') {
			for (let y = 0; y < this.height; y++) this.next[y].fill(' ');
			for (let y = 0; y < this.height; y++) {
				for (let x = 0; x < this.width; x++) {
					if (this.data[y][x] === 'X') {
						if (this.canMove(x,     y, time + 1)) this.next[y][x]     = 'X';
						if (this.canMove(x - 1, y, time + 1)) this.next[y][x - 1] = 'X';
						if (this.canMove(x + 1, y, time + 1)) this.next[y][x + 1] = 'X';
						if (this.canMove(x, y - 1, time + 1)) this.next[y - 1][x] = 'X';
						if (this.canMove(x, y + 1, time + 1)) this.next[y + 1][x] = 'X';
					}
				}
			}
			time++;
			for (let y = 0; y < this.height; y++) {
				for (let x = 0; x < this.width; x++) {
					this.data[y][x] = this.next[y][x];
				}
			}
		}
		return time;
	};
}

export default Map;
