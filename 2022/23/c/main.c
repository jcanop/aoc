#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <file_reader.h>
#include <map.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"

// --- Main function ---
int main(void) {
	// --- Variables ---
	Map* map = map_create();
	int len, x = 0, y = 0;

	// --- Read and parse the input file ---
	READ_BY_LINE_HEAD(file_t);
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE);
	char line[file.max];
	READ_BY_LINE_WHILE(file, line)
		len = strlen(line);
		for (x = 0; x < len; x++) {
			if (line[x] == '#') map_add_elf(map, x, y);
		}
		y++;
	READ_BY_LINE_DONE(file);

	// --- Puzzles ---
	while (map_round(map)) {
		// --- Puzzle 1 ---
		if (map->round == 10) {
			int total = map_empty_count(map);
			printf("1. Empty ground tiles: %d\n", total);
		}
	}

	// --- Puzzle 2 ---
	printf("2. First round with no Elf moves: %d\n", map->round);

	// --- Free resources ---
	map_free(map);

	return EXIT_SUCCESS;
}
