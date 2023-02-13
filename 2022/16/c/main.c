#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <file_reader.h>
#include <uthash.h>
#include <set.h>
#include <lib.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"

// --- Main function ---
int main(void) {
	// --- Variables ---
	READ_BY_LINE_HEAD(file_t)
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	int count = file.rows;
	char valves[count][ID_SIZE];
	char useful[count][ID_SIZE];
	Tunnels* tunnels = NULL;

	Layout layout;
	layout.flows = set_create();
	layout.distances = NULL;
	layout.useful = useful;
	layout.useful_len = 0;

	char line[file.max];
	int i = 0, j = 0, rate;
	char id[ID_SIZE], csv[file.max];

	// --- Read and parse the input file ---
	READ_BY_LINE_WHILE(file, line)
		sscanf(line, "Valve %s has flow rate=%d; %*s %*s to %*s %[^\n]", &id, &rate, &csv);

		// --- Valves ---
		strcpy(valves[i], id);

		// --- Flows ---
		set_put_flow(layout.flows, id, rate);

		// --- Useful ---
		if (rate > 0) {
			strcpy(layout.useful[layout.useful_len++], id);
		}

		// --- Tunnels ---
		j = 0;
		Tunnels* t = malloc(sizeof(Tunnels));
		strcpy(t->id, id);
		char* ptr = strtok(csv, ", ");
		while (ptr != NULL) {
			if (j == MAX_TO) {
				fprintf(stderr, "MAX_TO Overflow!");
				return EXIT_FAILURE;
			}
			strcpy(&t->leads[j++][0], ptr);
			ptr = strtok(NULL, ", ");
		}
		while (j < MAX_TO) t->leads[j++][0] = 0;
		HASH_ADD_STR(tunnels, id, t);
		i++;
	READ_BY_LINE_DONE(file)

	// --- Distances ---
	layout.distances = floyd_warshal(valves, count, tunnels);

	// --- Puzzle 1 ---
	int total = dfs1(&layout);
	printf("1. Total pressure released: %d\n", total);

	// --- Puzzle 2 ---
	total = dfs2(&layout);
	printf("2. Total pressure released with an elephant: %d\n", total);

	// --- Free Resources ---
	layout_free(&layout);

	Tunnels *t, *tmpt;
	HASH_ITER(hh, tunnels, t, tmpt) {
		HASH_DEL(tunnels, t);
		free(t);
	}

	return EXIT_SUCCESS;
}
