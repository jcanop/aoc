import { GEODE, loadBlueprints, createState, mine, build, canBuild, shouldBuild, shouldBuildSomething, clone } from "./lib.mjs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";

// --- Search for the max production ---
function getMax(bp, limit) {
	let max = 0;
	let queue = [];
	queue.push(createState(bp));

	while (queue.length > 0) {
		let state = queue.shift();
		while (state.time != limit && !shouldBuildSomething(state, limit)) mine(state);
		if (state.time === limit - 1) mine(state);
		if (state.time === limit) {
			max = Math.max(max, state.materials[GEODE]);
			continue;
		}

		if (canBuild(state, GEODE)) {
			mine(state);
			build(state, GEODE);
			queue.push(state);
			continue;
		}

		for (let i = 2; i >= 0; i--) {
			if (canBuild(state, i) && shouldBuild(state, i, limit)) {
				let c = clone(state);
				mine(c);
				build(c, i);
				queue.push(c);
				state.mask |= (1 << i);
			}
		}

		mine(state);
		queue.push(state);
	}
	return max;
}

// --- Read and parse the input file ---
let bps = loadBlueprints(INPUT_FILE);

// --- Puzzle 1 ---
let total = bps.map(bp => bp.id * getMax(bp, 24)).reduce((acc, val) => acc + val, 0);
console.log("1. Total sum of quality levels:", total.toLocaleString());

// --- Puzzle 2 ---
total = bps.slice(0, 3).map(bp => getMax(bp, 32)).reduce((acc, val) => acc * val, 1);
console.log("2. Total of multiply the first 3 blueprints max values:", total.toLocaleString());
