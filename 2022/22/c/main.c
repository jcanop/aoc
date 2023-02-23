#include <stdio.h>
#include <stdlib.h>
#include <map.h>
#include <lib.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"

// --- Main function ---
int main(void) {
	// --- Puzzle 1 ---
	Map* map = malloc(sizeof(Map));
	map_load(map, INPUT_FILE);
	map_path(map, move_grid);
	long total = map_password(map);
	printf("1. Password: %ld\n", total);
	map_free(map); map = NULL;

	// --- Puzzle 2 ---
	map = malloc(sizeof(Map));
	map_load(map, INPUT_FILE);
	map_path(map, move_cube);
	total = map_password(map);
	printf("2. Password: %ld\n", total);
	map_free(map); map = NULL;

	return EXIT_SUCCESS;
}
