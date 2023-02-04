#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"

// --- Macros ---
#define grid(y, x) *(data + y * w + x)

// --- Structs ---
READ_BY_LINE_HEAD(file_t)

// Checks if a tree is visible
bool is_visible(char* data, int w, int h, int x, int y) {
	int i;
	for (i = y + 1; i < h; i++) {
		if (grid(i, x) >= grid(y, x)) break;
		if (i == h - 1) return true;
	}
	for (i = y - 1; i >= 0; i--) {
		if (grid(i, x) >= grid(y, x)) break;
		if (i == 0) return true;
	}
	for (i = x + 1; i < w; i++) {
		if (grid(y, i) >= grid(y, x)) break;
		if (i == w - 1) return true;
	}
	for (i = x - 1; i >= 0; i--) {
		if (grid(y, i) >= grid(y, x)) return false;
		if (i == 0) return true;
	}
	fprintf(stderr, "Unreachable code!");
	exit(EXIT_FAILURE);
}

// Calculates the scenic score of a tree
int scenic_score(char* data, int w, int h, int x, int y) {
	int a = 1;
	for (int i = y + 1; i < h - 1; i++) {
		if (grid(i, x) >= grid(y, x)) break;
		a++;
	}
	int b = 1;
	for (int i = y - 1; i > 0; i--) {
		if (grid(i, x) >= grid(y, x)) break;
		b++;
	}
	int c = 1;
	for (int i = x + 1; i < w - 1; i++) {
		if (grid(y, i) >= grid(y, x)) break;
		c++;
	}
	int d = 1;
	for (int i = x - 1; i > 0; i--) {
		if (grid(y, i) >= grid(y, x)) break;
		d++;
	}
	return a * b * c * d;
}

// --- Main function ---
int main(void) {
	// --- Variables ---
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	char line[file.max];
	size_t width = file.max - 2;
	size_t len = width * file.rows;
	char* data = malloc(len);
	int idx = 0;

	// --- Read and parse the input file ---
	READ_BY_LINE_WHILE(file, line)
		memcpy((data + idx++ * width), line, width);
	READ_BY_LINE_DONE(file)

	// --- Puzzle 1 ---
	int total = (width + file.rows - 2) * 2;
	int y, x;
	for (y = 1; y < file.rows - 1; y++)
		for (x = 1; x < width - 1; x++)
			total += is_visible(data, width, file.rows, x, y) ? 1 : 0;
	printf("1. Trees that are visible from aoutside the grid: %d\n", total);

	// --- Puzzle 2 ---
	int max = 0;
	for (y = 1; y < file.rows - 1; y++) {
		for (x = 1; x < width - 1; x++) {
			int score = scenic_score(data, width, file.rows, x, y);
			if (score > max) max = score;
		}
	}
	printf("2. The highest scenic score is: %d\n", max);

	// --- Free resources ---
	free(data); data = NULL;

	return EXIT_SUCCESS;
}
