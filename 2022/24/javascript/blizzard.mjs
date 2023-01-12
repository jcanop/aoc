// --- Constants ---
const NORTH = '^';
const SOUTH = 'v';
const WEST  = '<';
const EAST  = '>';

// --- Blizzards going east and west ---
function Row() {
	this.east = [];
	this.west = [];

	this.add = function(dir) {
		switch(dir) {
			case EAST: this.east.push(true);  this.west.push(false); break;
			case WEST: this.east.push(false); this.west.push(true);  break;
			default:   this.east.push(false); this.west.push(false);
		}
	};

	this.isEmpty = function(x, time) {
		const len = this.east.length;
		const wi = (x - 1 + time) % len;
		const ei = (x - 1 + len - (time % len)) % len;
		return !this.east[ei] && !this.west[wi];
	};
}

// --- Blizzards going north and south ---
function Col() {
	this.north = [];
	this.south = [];

	this.add = function(dir) {
		switch(dir) {
			case NORTH: this.north.push(true);  this.south.push(false); break;
			case SOUTH: this.north.push(false); this.south.push(true);  break;
			default:    this.north.push(false); this.south.push(false);
		}
	};

	this.isEmpty = function(y, time) {
		const len = this.north.length;
		const ni = (y - 1 + time) % len;
		const si = (y - 1 + len - (time % len)) % len;
		return !this.north[ni] && !this.south[si];
	};
}

// --- Blizzard object ---
function Blizzard() {
	this.cols = [];
	this.rows = [];

	this.add = function(line) {
		const chars = line.split("");
		let row = new Row();
		while (this.cols.length < chars.length) this.cols.push(new Col());
		for (let x = 0; x < chars.length; x++) {
			const d = chars[x];
			let col = this.cols[x];
			col.add(d);
			row.add(d);
		}
		this.rows.push(row);
	};

	this.isEmpty = function(x, y, time) {
		return this.cols[x - 1].isEmpty(y, time) && this.rows[y - 1].isEmpty(x, time);
	};
}

export default Blizzard;
