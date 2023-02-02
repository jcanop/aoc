#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"

// --- Range functions ---
bool fully_contains(int a_min, int a_max, int b_min, int b_max) {
	return a_min <= b_min && b_max <= a_max;
}

bool overlap(int a_min, int a_max, int b_min, int b_max) {
	return  (b_min >= a_min && b_min <= a_max) ||
			(b_max >= a_min && b_max <= a_max) ||
			(a_min >= b_min && a_min <= b_max) ||
			(a_max >= b_min && a_max <= b_max);

}

// --- Main function ---
int main(void) {
	int totals[] = { 0, 0 };

	// --- Read and parse the input file ---
	READ_BY_LINE_HEAD(file_t)
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE);
	char line[file.max];
	READ_BY_LINE_WHILE(file, line)
		int a_min, a_max, b_min, b_max;
		sscanf(line, "%d-%d,%d-%d", &a_min, &a_max, &b_min, &b_max);

		// --- Puzzle 1 ---
		if (fully_contains(a_min, a_max, b_min, b_max) ||
			fully_contains(b_min, b_max, a_min, a_max)) totals[0]++;

		// --- Puzzle 2 ---
		if (overlap(a_min, a_max, b_min, b_max)) totals[1]++;
	READ_BY_LINE_DONE(file)

	printf("Part 1. Total of ranges that fully contains another: %d\n", totals[0]);
	printf("Part 2. Total of ranges that overlaps: %d\n", totals[1]);

	return EXIT_SUCCESS;
}
