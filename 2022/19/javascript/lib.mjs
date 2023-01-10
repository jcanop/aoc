import fs from "node:fs";

// --- Constants ---
const ORE = 0;
const CLAY = 1;
const OBSIDIAN = 2;
const GEODE = 3;
const REGEX = /Blueprint (\d+):\s+Each ore robot costs (\d+) ore.\s+Each clay robot costs (\d+) ore.\s+Each obsidian robot costs (\d+) ore and (\d+) clay.\s+Each geode robot costs (\d+) ore and (\d+) obsidian./g;

// --- Blueprint ---
function loadBlueprints(filename) {
	let list = [];

	// --- Reads and parse the input file ---
	const text = fs.readFileSync(filename, "utf8");
	for (const cap of text.trim().matchAll(REGEX)) {
		const id = cap[1];
		let costs = [];
		for (let i = 0; i < 4; i++) costs.push(Array(3).fill(0));
		costs[ORE][ORE] = Number(cap[2]);
		costs[CLAY][ORE] = Number(cap[3]);
		costs[OBSIDIAN][ORE] = Number(cap[4]);
		costs[OBSIDIAN][CLAY] = Number(cap[5]);
		costs[GEODE][ORE] = Number(cap[6]);
		costs[GEODE][OBSIDIAN] = Number(cap[7]);
		let maxCosts = Array(3).fill(0);
		for (let i = 0; i < 3; i++)
			for (let j = 0; j < 4; j++)
				maxCosts[i] = Math.max(maxCosts[i], costs[j][i]);
		list.push({ id, costs, maxCosts });
	}
	return list;
}

// --- State ---
function createState(bp) {
	let robots = [1, 0, 0, 0];
	let materials = Array(4).fill(0);
	let mask = 0;
	let time = 0;
	return { bp, robots, materials, mask, time };
}

function mine(state) {
	for (let i = 0; i < 4; i++) state.materials[i] += state.robots[i];
	state.time++;
}

function canBuild(state, robot) {
	for (let i = 0; i < 3; i++)
		if (state.materials[i] < state.bp.costs[robot][i]) return false;
	return true;
}

function shouldBuild(state, robot, limit) {
	if (robot === GEODE) return true;
	let m = (1 << robot);
	if ((state.mask & m) === m) return false;
	const max = state.bp.maxCosts[robot];
	if (state.materials[robot] >= (limit - state.time) * max) return false;
	return state.robots[robot] < max;
}

function shouldBuildSomething(state, limit) {
	for (let i = 3; i >= 0; i--)
		if (canBuild(state, i) && shouldBuild(state, i, limit)) return true;
	return false;
}

function build(state, robot) {
	for (let i = 0; i < 3; i++) state.materials[i] -= state.bp.costs[robot][i];
	state.robots[robot]++;
}

function clone(state) {
	return {
		bp: state.bp, robots: [...state.robots], materials: [...state.materials],
		mask: 0, time: state.time
	};
}

export { GEODE, loadBlueprints, createState, mine, build, canBuild, shouldBuild, shouldBuildSomething, clone };
