import fs from "node:fs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const C_PACKET = 4;
const C_MESSAGE = 14;

// Indicates if an array has unique characters.
function unique(data, offset, len) {
	for (let i = offset; i < offset + len; i++)
		for (let j = offset; j < offset + len; j++)
			if (i != j && data[i] === data[j])
				return false;
	return true;
}

// Search the stream of data for a marker.
function find(data, c) {
	let len = data.length;
	if (len < c) return - 1;
	for (let i = 0; i < len - c; i++)
		if (unique(data, i, c))
			return i + c;

	return -1;
}

// Search for the start of a packet.
function findPacket(data) {
	return find(data, C_PACKET);
}

// Search for the sstart of a message.
function findMessage(data) {
	return find(data, C_MESSAGE);
}


// --- Read the input file ---
const data = Array.from(fs.readFileSync(INPUT_FILE).toString());

// --- Puzzles ---
console.log("Part 1. Start-of-packet marker: ", findPacket(data).toLocaleString());
console.log("Part 2. Start-of-message marker: ", findMessage(data).toLocaleString());
