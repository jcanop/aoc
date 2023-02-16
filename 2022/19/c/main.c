#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <file_reader.h>
#include <lib.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"

// --- Macros ---
#define MAX(a, b, c, d) (a > b ? (a > c ? (a > d ? a : d) : c) : b)

// --- Main function ---
int main(void) {
	// --- Variables ---
	READ_BY_LINE_HEAD(file_t);
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	const int len = file.rows;
	Blueprint bps[len];
	int id, ore_ore, clay_ore, obs_ore, obs_clay, geo_ore, geo_obs;
	int i = 0, j = 0, k = 0;
	char line[file.max];

	// --- Read and parse the input file ---
	READ_BY_LINE_WHILE(file, line)
		sscanf(line, "Blueprint %d: Each ore robot costs %d ore. Each clay\
			robot costs %d ore. Each obsidian robot costs %d ore and %d clay.\
			Each geode robot costs %d ore and %d obsidian.", &id, &ore_ore,
			&clay_ore, &obs_ore, &obs_clay, &geo_ore, &geo_obs);

		bps[i].id = id;
		for (j = 0; j < 4; j++) for (k = 0; k < 3; k++) bps[i].costs[j][k] = 0;
		bps[i].costs[ORE][ORE] = ore_ore;
		bps[i].costs[CLAY][ORE] = clay_ore;
		bps[i].costs[OBSIDIAN][ORE] = obs_ore;
		bps[i].costs[OBSIDIAN][CLAY] = obs_clay;
		bps[i].costs[GEODE][ORE] = geo_ore;
		bps[i].costs[GEODE][OBSIDIAN] = geo_obs;
		bps[i].max_ore = MAX(ore_ore, clay_ore, obs_ore, geo_ore);
		i++;
	READ_BY_LINE_DONE(file)

	// --- Puzzle 1 ---
	int total = 0;
	for (i = 0; i < len; i++) total += bps[i].id * get_max(&bps[i], 24);
	printf("1. Total sum of quality levels: %d\n", total);

	total = 1;
	for (i = 0; i < 3; i++) total *= get_max(&bps[i], 32);
	printf("2. Total of multiply the first 3 blueprints max values: %d\n", total);

	return EXIT_SUCCESS;
}
