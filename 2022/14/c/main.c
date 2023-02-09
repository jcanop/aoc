#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <map.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"

#define ORIGIN '+'
#define AIR ' '
#define ROCK '#'
#define SAND 'o'
#define OFFSET 2
#define ORIGIN_X 500
#define ORIGIN_Y 0

// Gets a title from the map
char get(struct MapNodeRoot* map, int height, int x, int y) {
	if (y == height - 1) return ROCK;
	char* tile = map_get(map, x, y);
	if (tile == NULL) {
		if (x == ORIGIN_X && y == ORIGIN_Y) return ORIGIN;
		return AIR;
	}
	return *tile;
}

// Counts the sand tiles
int count_sand_tiles(struct MapNodeRoot* map) {
	int x, y, total = 0;
	char* tile;
	for (x = map->min_x; x <= map->max_x; x++) {
		for (y = map->min_y; y <= map->max_y; y++) {
			tile = map_get(map, x, y);
			if (tile != NULL && *tile == SAND) total++;
		}
	}
	return total;
}

// Runs the simulation
void sim(struct MapNodeRoot* map, int height, int puzzle) {
	int x, y;
	while (1) {
		x = ORIGIN_X; y = ORIGIN_Y;
		while (1) {
			if (puzzle == 1 && y + 1 == height - 1) return;
			if (get(map, height, x,     y + 1) == AIR) { y++;      continue; }
			if (get(map, height, x - 1, y + 1) == AIR) { y++; x--; continue; }
			if (get(map, height, x + 1, y + 1) == AIR) { y++; x++; continue; }
			map_put(map, x, y, SAND);
			if (x == ORIGIN_X && y == ORIGIN_Y) return;
			break;
		}
	}
}

// --- Main function ---
int main(void) {
	// --- Variables ---
	struct MapNodeRoot* map = map_create();
	int height = 0;

	READ_BY_LINE_HEAD(file_t)
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	char line[file.max];
	int i, x, y, last_x, last_y, a, b;
	bool first;

	// --- Read and parse the input file ---
	READ_BY_LINE_WHILE(file, line)
		first = true;
		char* ptr = strtok(line, " -> ");
		while (ptr != NULL) {
			sscanf(ptr, "%d,%d", &x, &y);
			if (first) {
				first = false;
			} else {
				if (x != last_x) {
					if (x < last_x) { a = x; b = last_x; } else { a = last_x; b = x; }
					for (i = a; i <= b; i++) map_put(map, i, y, ROCK);
				}
				if (y != last_y) {
					if (y < last_y) { a = y; b = last_y; } else { a = last_y; b = y; }
					for (i = a; i <= b; i++) map_put(map, x, i, ROCK);
				}
			}
			last_x = x; last_y = y;
			ptr = strtok(NULL, " -> ");
		}
	READ_BY_LINE_DONE(file)
	height = map->max_y + 3;

	// --- Puzzle 1 --
	sim(map, height, 1);
	int total = count_sand_tiles(map);
	printf("1. Total units of sand: %d\n", total);

	// --- Puzzle 2 --
	sim(map, height, 2);
	total = count_sand_tiles(map);
	printf("2. Total units of sand: %d\n", total);

	// --- Free resources ---
	map_free(map);

	return EXIT_SUCCESS;
}
