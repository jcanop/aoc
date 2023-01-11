function Grove(text) {
	this.elves = new Set();
	this.proposed = {};
	this.banned = new Set();
	this.round = 0;

	for (const [y, line] of text.trim().split("\n").entries()) {
		for (const [x, c] of line.split("").entries()) {
			if (c === '#') this.elves.add(str(x, y));
		}
	}

	// --- Simulates one full round ---
	this.sim = function() {
		let result = false;

		// --- Phase 1: Considers move ---
		for (const position of this.elves) {
			const pos = point(position);
			let found = false;
			for (let y = -1; y <= 1; y++) {
				for (let x = -1; x <= 1; x++) {
					if (x === 0 && y === 0) continue;
					const p = str(pos.x, pos.y, x, y);
					if (this.elves.has(p)) {
						found = true;
						break;
					}
				}
				if (found) break;
			}
			if (!found) {
				// --- No Elf adjacent, then do nothing ---
				this.proposed[position] = position;
				continue;
			}

			// --- Look at the 4 directions ---
			let p = null;
			for (let n = 0; n < 4; n++) {
				let i = (n + this.round) % 4;
				if (i === 0) { // North
					if (this.elves.has(str(pos.x, pos.y, -1, -1)) ||
						this.elves.has(str(pos.x, pos.y,  0, -1)) ||
						this.elves.has(str(pos.x, pos.y,  1, -1))) continue;
					p = str(pos.x, pos.y, 0,  -1);
					break;
				} else if (i === 1) { // South
					if (this.elves.has(str(pos.x, pos.y, -1,  1)) ||
						this.elves.has(str(pos.x, pos.y,  0,  1)) ||
						this.elves.has(str(pos.x, pos.y,  1,  1))) continue;
					p = str(pos.x, pos.y, 0, 1);
					break;
				} else if (i === 2) { // West
					if (this.elves.has(str(pos.x, pos.y, -1, -1)) ||
						this.elves.has(str(pos.x, pos.y, -1,  0)) ||
						this.elves.has(str(pos.x, pos.y, -1,  1))) continue;
					p = str(pos.x, pos.y, -1, 0);
					break;
				} else if (i === 3) { // East
					if (this.elves.has(str(pos.x, pos.y,  1, -1)) ||
						this.elves.has(str(pos.x, pos.y,  1,  0)) ||
						this.elves.has(str(pos.x, pos.y,  1,  1))) continue;
					p = str(pos.x, pos.y, 1, 0);
					break;
				} else {
					throw "Unreachable!";
				}
			}

			if (p != null) {
				if (this.banned.has(p)) {
					this.proposed[position] = position;
				} else if (this.proposed.hasOwnProperty(p)) {
					this.banned.add(p);
					const e = this.proposed[p];
					delete this.proposed[p];
					this.proposed[e] = [e];
					this.proposed[position] = position;
				} else {
					this.proposed[p] = position;
				}
				result = true;
			} else {
				this.proposed[position] = position;
			}
		}

		// --- Phase 2: Move ---
		this.elves.clear();
		for (const position of Object.keys(this.proposed)) {
			this.elves.add(position);
		}
		this.proposed = {};
		this.banned.clear();
		this.round++;

		return result;
	};

	// --- Calculates the view rectangle ---
	this.findViewRect = function() {
		let minX = Number.MAX_VALUE;
		let minY = Number.MAX_VALUE;
		let maxX = 0;
		let maxY = 0;
		for (const pos of this.elves) {
			const p = point(pos);
			minX = Math.min(minX, p.x);
			minY = Math.min(minY, p.y);
			maxX = Math.max(maxX, p.x);
			maxY = Math.max(maxY, p.y);
		}
		return [ minX, minY, maxX, maxY ];
	};

	// --- Count the empty tiles --
	this.emptyCount = function() {
		let total = 0;
		let [ minX, minY, maxX, maxY ] = this.findViewRect();
		for (let y = minY; y <= maxY; y++) {
			for (let x = minX; x <= maxX; x++) {
				if (!this.elves.has(str(x, y))) total++;
			}
		}
		return total;
	};

	// --- Prints the grove map ---
	this.print = function() {
		let elves = 0;
		let [ minX, minY, maxX, maxY ] = this.findViewRect();
		for (let y = minY; y <= maxY; y++) {
			let line = "";
			for (let x = minX; x <= maxX; x++) {
				if (this.elves.has(str(x, y))) { elves++; line += "#"; }
				else line += ".";
			}
			console.log(line);
		}
		console.log("Elves: ", elves);
	}


}

// --- Helper functions ---
function str(x, y, dx = 0, dy = 0) {
	return (x + dx) + "," + (y + dy);
}

function point(str) {
	const i = str.indexOf(",");
	const x = Number(str.substring(0, i));
	const y = Number(str.substring(i + 1));
	return { x, y };
}

export default Grove;
