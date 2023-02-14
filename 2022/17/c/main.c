#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <file_reader.h>
#include <lib.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define LIMIT_1 2022L
#define LIMIT_2 1000000000000L

// Runs the simulations
long sim(char* flows, long limit) {
	int flows_len = strlen(flows);
	long count = 0;
	long rocks = 1;
	long gcd = SHAPE_LEN * flows_len;
	Chamber chamber = { .root = row_create_node(), .shape_len = 0 };
	chamber_add_shape(&chamber, 0);

	// --- Variables for tracking a pattern ---
	long prev_height = 0;
	long prev_rocks = 0;
	long delta_height = 0;
	long delta_rocks = 0;
	long simulated = 0;
	long height, dh, dr, rate;

	// --- Iterate until the limit is reached ---
	while (1) {
		// --- Tries to find a pattern ---
		if (count > 0 && count % gcd == 0) {
			height = chamber_height(&chamber);
			dh = height - prev_height;
			dr = rocks - prev_rocks;

			// --- Pattern found ---
			if (dh == delta_height && dr == delta_rocks) {
				rate = (limit - rocks) / dr;
				simulated = dh * rate + 1;
				rocks = limit - ((limit - rocks) % dr);
			}
			prev_height = height;
			prev_rocks = rocks;
			delta_height = dh;
			delta_rocks = dr;
		}

		// --- Normal simulation ---
		chamber_move(&chamber, flows[count % flows_len]);
		if (!chamber_move(&chamber, DOWN)) {
			if (rocks > limit) {
				height = chamber_height(&chamber) + simulated;
				chamber_free(&chamber);
				return height;
			}
			chamber_add_shape(&chamber, rocks % SHAPE_LEN);
			rocks++;
		}
		count++;
	}
}

// --- Main function ---
int main(void) {
	// --- Read the input file ---
	char* text = NULL;
	READ_FULL(text, INPUT_FILE);
	int len = strlen(text);
	if (text[len - 1] == '\n') text[len - 1] = 0;

	// --- Puzzle 1 ---
	long height = sim(text, LIMIT_1);
	printf("1. The tower is %ld units tall\n", height);

	// --- Puzzle 2 ---
	height = sim(text, LIMIT_2);
	printf("2. The tower is %ld units tall\n", height);

	// --- Free resources ---
	free(text); text = NULL;

	return EXIT_SUCCESS;
}
