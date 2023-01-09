
function createSensor(x, y, bx, by) {
	const range = Math.abs(x - bx) + Math.abs(y - by);
	const isInRange = function(x, y) {
		return Math.abs(this.x - x) + Math.abs(this.y - y) <= this.range;
	}
	return { x, y, range, isInRange };
}

function createMap() {
	let sensors = {};
	let beacons = [];

	const addSensor = function(x, y, bx, by) {
		this.sensors[x + "," + y] = createSensor(x, y, bx, by);
		this.beacons.push(bx + "," + by);
	}

	const isInSensorRange = function(x, y) {
		for (const s of Object.values(this.sensors)) {
			if (s.isInRange(x, y)) return true;
		}
		return false;
	}

	const isEmpty = function(x, y) {
		const id = x + "," + y;
		return !this.sensors.hasOwnProperty(id) && !beacons.includes(id);
	}

	return { sensors, beacons, addSensor, isInSensorRange, isEmpty };
}

export default createMap;
