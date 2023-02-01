#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"

// --- Range functions ---
bool fully_contains(a_min, a_max, b_min, b_max) {
	return a_min <= b_min && b_max <= a_max;
}

bool overlap(a_min, a_max, b_min, b_max) {
	return  (b_min >= a_min && b_min <= a_max) ||
			(b_max >= a_min && b_max <= a_max) ||
			(a_min >= b_min && a_min <= b_max) ||
			(a_max >= b_min && a_max <= b_max);

}

// --- Main function ---
int main(void) {
	int totals[] = { 0, 0 };

	// --- Read and parse the input file ---
	READ_BY_LINE_INIT(line, rows, INPUT_FILE);
	READ_BY_LINE_WHILE(line);
		char* ptr = &line[0];
		while (*ptr != 0) {
			if (*ptr == ',') {
				*ptr = '-';
				break;
			}
			ptr++;
		}
		ptr = strtok(line, "-"); int a_min = atoi(ptr);
		ptr = strtok(NULL, "-"); int a_max = atoi(ptr);
		ptr = strtok(NULL, "-"); int b_min = atoi(ptr);
		ptr = strtok(NULL, "-"); int b_max = atoi(ptr);

		// --- Puzzle 1 ---
		if (fully_contains(a_min, a_max, b_min, b_max) ||
			fully_contains(b_min, b_max, a_min, a_max)) totals[0]++;

		// --- Puzzle 2 ---
		if (overlap(a_min, a_max, b_min, b_max)) totals[1]++;
	READ_BY_LINE_DONE();

	printf("Part 1. Total of ranges that fully contains another: %d\n", totals[0]);
	printf("Part 2. Total of ranges that overlaps: %d\n", totals[1]);

	return EXIT_SUCCESS;
}
