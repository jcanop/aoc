#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define NORTH '^'
#define SOUTH 'v'
#define WEST  '<'
#define EAST  '>'
#define EMPTY ' '
#define MARK  'X'

// Prints the grid map into the console
void map_print(char* grid[], int h, int w) {
	int y; for (y = 0; y < h; y++) {
		printf("%s\n", grid[y]);
	}
}

// Check if you can move into a place at a determined time
bool map_can_move(char* grid[], int h, int w, int x, int y, int time) {
	if (x == 1 && y == 0) return true; // Start position
	if (x == w - 2 && y == h - 1) return true; // End position
	if (x < 1 || x >= w - 1 || y < 1 || y >= h - 1) return false; // Border
	int len = w - 2;
	int wi = (x - 1 + time) % len + 1;
	int ei = (x - 1 + len - (time % len)) % len + 1;
	len = h - 2;
	int ni = (y - 1 + time) % len + 1;
	int si = (y - 1 + len - (time % len)) % len + 1;
	if (grid[y][wi] == WEST ||
		grid[y][ei] == EAST ||
		grid[ni][x] == NORTH ||
		grid[si][x] == SOUTH) return false;
	return true;
}

// Search the quickest way
int map_search(char* grid[], int h, int w, int sx, int sy, int ex, int ey, int time) {
	char data[h][w];
	char next[h][w];
	int x, y;
	for (y = 0; y < h; y++) {
		for (x = 0; x < w; x++) {
			data[y][x] = EMPTY;
			next[y][x] = EMPTY;
		}
	}
	data[sy][sx] = MARK;

	while (data[ey][ex] != MARK) {
		for (y = 0; y < h; y++) for (x = 0; x < w; x++) next[y][x] = EMPTY;
		for (y = 0; y < h; y++) {
			for (x = 0; x < w; x++) {
				if (data[y][x] == MARK) {
					if (map_can_move(grid, h, w, x,     y, time + 1)) next[y][x]     = MARK;
					if (map_can_move(grid, h, w, x - 1, y, time + 1)) next[y][x - 1] = MARK;
					if (map_can_move(grid, h, w, x + 1, y, time + 1)) next[y][x + 1] = MARK;
					if (map_can_move(grid, h, w, x, y - 1, time + 1)) next[y - 1][x] = MARK;
					if (map_can_move(grid, h, w, x, y + 1, time + 1)) next[y + 1][x] = MARK;
				}
			}
		}
		time++;
		for (y = 0; y < h; y++) {
			for (x = 0; x < w; x++) {
				data[y][x] = next[y][x];
			}
		}
	}
	return time;
}

// --- Main function ---
int main(void) {
	// --- Variables ---
	READ_BY_LINE_HEAD(file_t);
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE);
	const int width = file.max - 2;
	const int height = file.rows;
	const int start_x = 1, start_y = 0;
	const int end_x = width - 2, end_y = height - 1;

	char line[file.max];
	char* grid[height];
	int x, y = 0, len;

	// --- Read and parse the input file ---
	READ_BY_LINE_WHILE(file, line)
		grid[y] = malloc(width + 1);
		strcpy(grid[y++], line);
	READ_BY_LINE_DONE(file);

	// --- Puzzle 1 ---
	int time = map_search(grid, height, width, start_x, start_y, end_x, end_y, 0);
	printf("1. Minutes to reach the goal: %d\n", time);

	// --- Puzzle 2 ---
	time = map_search(grid, height, width, end_x, end_y, start_x, start_y, time);
	time = map_search(grid, height, width, start_x, start_y, end_x, end_y, time);
	printf("2. Minutes to reach the goal, go back to the start, then reach the goal again: %d\n", time);

	// --- Free resources ---
	for (y = 0; y < height; y++) {
		free(grid[y]); grid[y] = NULL;
	}

	return EXIT_SUCCESS;
}
