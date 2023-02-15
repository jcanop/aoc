#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <queue.h>
#include <file_reader.h>
#include <set.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"

// --- Macros ---
#define MAX(m, a) if (a > m) m = a

// --- Types ---
typedef unsigned int uint;

// --- Structs ---
typedef struct Point {
	unsigned int x;
	unsigned int y;
	unsigned int z;
} Point;

typedef struct QueueNode {
	unsigned int x;
	unsigned int y;
	unsigned int z;
	TAILQ_ENTRY(QueueNode) nodes;
} QueueNode;

READ_BY_LINE_HEAD(file_t);

// Calcualtes the surface
uint calculate(uint len_x, uint len_y, uint len_z, const char map[len_x][len_y][len_z],  uint len, const Point list[len]) {

	uint i, x, y, z, total = 0;
	for (i = 0; i < len; i++) {
		x = list[i].x; y = list[i].y; z = list[i].z;
		total += 6;
		if (x > 0 && map[x - 1][y][z] == 1) total--;
		if (y > 0 && map[x][y - 1][z] == 1) total--;
		if (z > 0 && map[x][y][z - 1] == 1) total--;
		if (x < len_x - 1 && map[x + 1][y][z] == 1) total--;
		if (y < len_y - 1 && map[x][y + 1][z] == 1) total--;
		if (z < len_z - 1 && map[x][y][z + 1] == 1) total--;
	}
	return total;
}

// Depth First Search
bool dfs(uint len_x, uint len_y, uint len_z, const char map[len_x][len_y][len_z], uint x, uint y, uint z, Set* closed) {
	// --- Variables ---
	TAILQ_HEAD(QueueHead, QueueNode) queue;
	QueueNode* node;
	bool result = false;

	// Internal functions that insert a position into the queue
	void add_queue(struct QueueHead* queue, uint x, uint y, uint z) {
		node = malloc(sizeof(QueueNode));
		node->x = x; node->y = y; node->z = z;
		TAILQ_INSERT_TAIL(queue, node, nodes);
		node = NULL;
	}

	// --- Init Queue and Visited Set ---
	TAILQ_INIT(&queue);
	add_queue(&queue, x, y, z);

	Set* visited = set_create();
	set_add(visited, x, y, z);

	// ---- DFS ---
	while (!TAILQ_EMPTY(&queue)) {
		node = TAILQ_FIRST(&queue);
		TAILQ_REMOVE(&queue, node, nodes);
		x = node->x; y = node->y; z = node->z;
		free(node);
		node = NULL;

		if (x == 0 || x == len_x - 1 ||
			y == 0 || y == len_y - 1 ||
			z == 0 || z == len_z - 1) {

			result = true;
			goto finish;
		}

		if (map[x - 1][y][z] == 0 && !set_contains(visited, x - 1, y, z)) { set_add(visited, x - 1, y, z); add_queue(&queue, x - 1, y, z); }
		if (map[x + 1][y][z] == 0 && !set_contains(visited, x + 1, y, z)) { set_add(visited, x + 1, y, z); add_queue(&queue, x + 1, y, z); }
		if (map[x][y - 1][z] == 0 && !set_contains(visited, x, y - 1, z)) { set_add(visited, x, y - 1, z); add_queue(&queue, x, y - 1, z); }
		if (map[x][y + 1][z] == 0 && !set_contains(visited, x, y + 1, z)) { set_add(visited, x, y + 1, z); add_queue(&queue, x, y + 1, z); }
		if (map[x][y][z - 1] == 0 && !set_contains(visited, x, y, z - 1)) { set_add(visited, x, y, z - 1); add_queue(&queue, x, y, z - 1); }
		if (map[x][y][z + 1] == 0 && !set_contains(visited, x, y, z + 1)) { set_add(visited, x, y, z + 1); add_queue(&queue, x, y, z + 1); }

	}
	set_add_all(closed, visited);
	result = false;

finish:
	// --- Free Resources ---
	set_free(visited);
	while (!TAILQ_EMPTY(&queue)) {
		node = TAILQ_FIRST(&queue);
		TAILQ_REMOVE(&queue, node, nodes);
		free(node);
		node = NULL;
	}

	return result;
}

// --- Main function ---
int main(void) {
	// --- Variables ---
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	const int len = file.rows;
	Point list[len];
	char line[file.max];
	uint i = 0, x, y, z, max_x = 0, max_y = 0, max_z = 0;

	// --- Read and parse the input file ---
	READ_BY_LINE_WHILE(file, line)
		sscanf(line, "%d,%d,%d", &x, &y, &z);
		list[i].x = x; list[i].y = y; list[i].z = z; i++;
		MAX(max_x, x); MAX(max_y, y); MAX(max_z, z);
	READ_BY_LINE_DONE(file)

	// --- Init 3D map ---
	char map[max_x + 1][max_y + 1][max_z + 1];
	for (x = 0; x <= max_x; x++) {
		for (y = 0; y <= max_y; y++) {
			for (z = 0; z <= max_z; z++) {
				map[x][y][z] = 0;
			}
		}
	}
	for (i = 0; i < len; i++) {
		x = list[i].x; y = list[i].y; z = list[i].z;
		map[x][y][z] = 1;
	}

	// --- Puzzle 1 ---
	uint total = calculate(max_x + 1, max_y + 1, max_z + 1, map, len, list);
	printf("1. Total surface: %d\n", total);

	// --- Puzzle 2 ---
	Set* closed = set_create();
	for (x = 1; x < max_x; x++) {
		for (y = 1; y < max_y; y++) {
			for (z = 1; z < max_z; z++) {
				if (map[x][y][z] == 1) continue;
				if (!set_contains(closed, x, y, z)) {
					dfs(max_x + 1, max_y + 1, max_z + 1, map, x, y, z, closed);
				}
			}
		}
	}

	void update_map(uint x, uint y, uint z) {
		map[x][y][z] = 1;
	}
	set_do(closed, update_map);

	total = calculate(max_x + 1, max_y + 1, max_z + 1, map, len, list);
	printf("2. Adjusted total surface: %d\n", total);

	// --- Free resources ---
	set_free(closed);

	return EXIT_SUCCESS;
}
