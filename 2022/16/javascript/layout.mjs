import fs from "node:fs";

// --- Constants ---
const REGEX = /Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z]{2}(,\s*[A-Z]{2})*)/g

// --- Creates a new Layout from the input file ---
function createLayout(filename) {
	// --- Variables ---
	let valves = new Set();
	let flows = {};
	let tunnels = {};
	let usefulValves = new Set();

	// --- Reads and parse the input file ---
	const text = fs.readFileSync(filename, "utf8");
	for (const cap of text.matchAll(REGEX)) {
		let id = cap[1];
		let flow = Number(cap[2]);
		let routes = cap[3].split(/\s*,\s*/);
		valves.add(id);
		flows[id] = flow;
		tunnels[id] = routes;
		if (flow > 0) usefulValves.add(id);
	}
	const all = FW(valves, tunnels);

	const getFlow = function(set) {
		let total = 0;
		for (const x of set) total += this.flows[x];
		return total;
	}

	const dfs1 = function() { return DFS1(this, "AA", 0, 0, new Set()); }
	const dfs2 = function() { return DFS2(this, "AA", false, 0, 0, new Set(), this.usefulValves, 0); }

	return { flows, usefulValves, all, getFlow, dfs1, dfs2 };
}

// --- Floyd Warshall Algorithm ---
function FW(valves, tunnels) {
	let distances = {};
	for (const from of valves.values()) {
		let map = {};
		for (const to of valves.values()) {
			map[to] = valves.size + 1;
		}
		distances[from] = map;
	}

	for (const from of Object.keys(tunnels)) {
		for (const to of tunnels[from]) {
			distances[from][to] = 1;
		}
	}

	for (const from of valves.values()) {
		distances[from][from] = 0;
	}

	for (const via of valves.values()) {
		for (const from of valves.values()) {
			for (const to of valves.values()) {
				let d = distances[from][via] + distances[via][to];
				if (distances[from][to] > d) {
					distances[from][to] = d;
				}
			}
		}
	}
	return distances;
}

// --- Depth First Search: Puzzle 1 ---
function DFS1(map, current, time, total, open) {
	let max = total + map.getFlow(open) * (30 - time);
	for (const next of map.usefulValves) {
		if (open.has(next)) continue;
		const delta = map.all[current][next] + 1;
		if (time + delta >= 30) continue;
		let newTotal = total + delta * map.getFlow(open);
		open.add(next);
		const value = DFS1(map, next, time + delta, newTotal, open);
		if (max < value) max = value;
		open.delete(next);
	}
	return max;
}

// --- Depth First Search: Puzzle 2 ---
function DFS2(map, current, elephant, time, total, open, useful, totalFlow) {
	let max = total + totalFlow * (26 - time);
	if (!elephant) {
		let newCandidates = new Set(useful);
		for (const v of open) newCandidates.delete(v);
		let newOpen = new Set();
		let maxElephant = DFS2(map, "AA", true, 0, 0, newOpen, newCandidates, 0);
		max = total + totalFlow * (26 - time) + maxElephant;
	}

	for (const next of useful) {
		if (open.has(next)) continue;
		const delta = map.all[current][next] + 1;
		if (time + delta >= 26) continue;
		const newTotal = total + delta * totalFlow
		open.add(next);
		const nextFlow = map.flows[next];
		totalFlow += nextFlow;
		const value = DFS2(map, next, elephant, time + delta, newTotal, open, useful, totalFlow);
		if (max < value) max = value;
		open.delete(next);
		totalFlow -= nextFlow;
	}
	return max;
}

export default createLayout;
