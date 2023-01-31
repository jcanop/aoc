#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "./lib.h"

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define LINE_MAX_LEN 32U

// --- Main function ---
int main(void) {
	int total[] = { 0, 0 };

	// --- Read and parse the input file ---
	char line[LINE_MAX_LEN];
	FILE* file = fopen(INPUT_FILE, "r");
	if (file == NULL) {
		printf("Not able to open the input file.");
		return EXIT_FAILURE;
	}

	while(fgets(line, LINE_MAX_LEN, file)) {
		// --- Puzzle 1 ---
		enum Shape other = parse_shape(line[0]);
		enum Shape me = parse_shape(line[2]);
		enum Result result = play(me, other);
		total[0] += result + me;

		// --- Puzzle 2 ---
		result = parse_result(line[2]);
		me = shape_to(other, result);
		total[1] += result + me;
	}
	fclose(file);

	// --- Puzzle 1 ---
	printf("Part 1. Total score: %d.\n", total[0]);

	// --- Puzzle 2 ---
	printf("Part 2. Total score: %d.\n", total[1]);

	return EXIT_SUCCESS;
}
