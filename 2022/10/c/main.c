#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define BUFFER_LEN 250

// --- Macro ---
#define add_buffer(c)\
	buffer[idx++] = c;\
	if (idx == BUFFER_LEN) {\
		fprintf(stderr, "Buffer overflow");\
		exit(EXIT_FAILURE);\
	}\

// --- Structs ---
READ_BY_LINE_HEAD(file_t)

// --- Main function ---
int main(void) {
	// --- Variables ---
	int registry = 1;
	int cycle = 1;
	int mark = 20;
	int total = 0;
	char buffer[BUFFER_LEN];
	int idx = 0;
	int i, count;

	// --- Read and parse the input file ---
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	char line[file.max];
	READ_BY_LINE_WHILE(file, line)
		count = (strcmp(line, "noop") == 0) ? 1 : 2;
		for (i = 0; i < count; i++) {
			// --- Puzzle 1 ---
			if (cycle == mark) {
				total += registry * mark;
				mark += 40;
			}

			// --- Puzzle 2 ---
			int p = (cycle - 1) % 40;
			char c = (p >= registry - 1 && p <= registry + 1) ? '#' : ' ';
			add_buffer(c);
			if (cycle % 40 == 0) add_buffer('\n');

			// --- Both ---
			cycle++;
			if (count == 2 && i == 1) {
				int n; sscanf(line, "addx %d", &n);
				registry += n;
			}
		}
	READ_BY_LINE_DONE(file)
	add_buffer(0);

	// --- Puzzle 1 ---
	printf("1. The sum of the signal strenghts is: %d\n", total);

	// --- Puzzle 2 ---
	printf("2. Image on the CRT\n%s\n", &buffer[0]);

	return EXIT_SUCCESS;
}
