import fs from "node:fs";
import readline from "node:readline";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const LIMIT = 100_000;
const DISK_SIZE = 70_000_000;
const UPDATE_SIZE = 30_000_000;

// Creates a new Root node.
function newRoot() {
	return { name: "/", size: 0, childs: {} };
}

// Creates a new Dir node.
function newDir(parent, name) {
	return { name, size: 0, parent, childs: {} };
}

// Creates a new File node.
function newFile(parent, name, size) {
	return { name, size, parent };
}

// This recursive method updates all the directories sizes according to their content.
function updateDirSizes(node) {
	for (const name in node.childs) {
		const c = node.childs[name];
		if (c.childs) updateDirSizes(c);
		node.size += c.size;
	}
}

// This recursive method searches all directories smaller in size than a limit.
function sumSmallDirs(node, limit) {
	let total = 0;
	for (const name in node.childs) {
		const c = node.childs[name];
		if (c.childs) {
			if (c.size <= limit) total += c.size;
			total += sumSmallDirs(c, limit);
		}
	}
	return total;
}

// This recursive method searches for the smaller directory over a limit.
function findDirToDelete(node, need) {
	let result = -1;
	for (const name in node.childs) {
		const c = node.childs[name];
		if (c.childs) {
			const i = findDirToDelete(c, need);
			if (i !== -1 && (result === -1 || result > i)) result = i;
		}
		if (c.size >= need && (result === -1 || result > c.size)) result = c.size;
	}
	return result;
}

let root = newRoot();
let current = root;

// --- Read and parse the input file ---
const stream = fs.createReadStream(INPUT_FILE);
const reader = readline.createInterface({ input: stream, crlDelay: Infinity });
reader.on("line", line => {
	if (line.startsWith("$ cd ")) {
		const name = line.substring(5);
		switch (name) {
			case "/": current = root; break;
			case "..": current = current.parent; break;
			default: current = current.childs[name]; break;
		}
	} else if (line === "$ ls") {
		// Ignore command
	} else if (line.startsWith("dir ")) {
		const name = line.substring(4);
		const dir = newDir(current, name);
		current.childs[name] = dir;
	} else {
		const i = line.indexOf(' ');
		const size = Number(line.substring(0, i));
		const name = line.substring(i + 1);
		const file = newFile(current, name, size);
		current.childs[name] = file;
	}
});
await new Promise(res => reader.once("close", res));
updateDirSizes(root);

// --- Puzzle 1 ---
const total =  sumSmallDirs(root, LIMIT).toLocaleString();
console.log(`1. The sum of the total sizes of the directories under ${LIMIT} is ${total}`);

// --- Puzzle 2 ---
const need = UPDATE_SIZE - (DISK_SIZE - root.size);
const size = findDirToDelete(root, need).toLocaleString();
console.log(`2. The total size of the smalles directory needed is ${size}`);
