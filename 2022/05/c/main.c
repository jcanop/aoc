#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <sys/queue.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"

// --- Queue Node ---
struct node {
	char c;
	TAILQ_ENTRY(node) nodes;
};
TAILQ_HEAD(head_s, node);

// --- Find out how many stacks of crates are defined ---
int find_stacks_count() {
	int count = 0;
	READ_BY_LINE_INIT(line, rows, INPUT_FILE);
	READ_BY_LINE_WHILE(line);
		int len = strlen(line) + 1;
		count = len / 4;
		break;
	READ_BY_LINE_DONE();
	return count;
}

// --- Add an element to a queue ---
int queue_add(struct head_s* head, char c, bool top) {
	struct node* e = malloc(sizeof(struct node));
	if (e == NULL) {
		fprintf(stderr, "Malloc failed");
		exit(EXIT_FAILURE);
	}
	e->c = c;
	if (top)
		TAILQ_INSERT_HEAD(head, e, nodes);
	else
		TAILQ_INSERT_TAIL(head, e, nodes);
	e = NULL;
	return 0;
}

// --- Pop an element from a queue ---
char queue_pop(struct head_s* head) {
	if (TAILQ_EMPTY(head)) {
		fprintf(stderr, "Empty Queue!");
		exit(EXIT_FAILURE);
	}
	struct node* e = TAILQ_FIRST(head);
	TAILQ_REMOVE(head, e, nodes);
	char c = e->c;
	free(e);
	e = NULL;
	return c;
}

// --- Free the memory used by a queue ---
int queue_free(struct head_s* head) {
	struct node* e = NULL;
	while(!TAILQ_EMPTY(head)) {
		e = TAILQ_FIRST(head);
		TAILQ_REMOVE(head, e, nodes);
		free(e);
		e = NULL;
	}
	return 0;
}

// --- Main function ---
int main(void) {
	int count = find_stacks_count();

	// --- Init queues for both puzzles ---
	// The queues at [0][X] are for puzzle 1, and the ones at [1][X] are for puzzle 2.
	// The queue at [1][0] is used as buffer, the one at [0][0] is not used.
	struct head_s heads[2][count + 1];
	int i, j;
	for (i = 0; i < 2; i++)
		for (j = 0; j <= count; j++)
			TAILQ_INIT(&heads[i][j]);

	// --- Read and parse the input file ---
	bool setup = true;
	READ_BY_LINE_INIT(line, rows, INPUT_FILE);
	READ_BY_LINE_WHILE(line);
		if (setup) {
			// --- Loads the containers into the queues ---
			if (strlen(line) == 0) {
				setup = false;
			} else if (strchr(line, '[') != NULL) {
				for (i = 0; i < count; i++) {
					char c = line[4 * i + 1];
					if (c != ' ') {
						queue_add(&heads[0][i + 1], c, false);
						queue_add(&heads[1][i + 1], c, false);
					}
				}
			}
			continue;
		}

		// --- Moves the containers ---
		int cnt, from, to;
		sscanf(line, "move %d from %d to %d", &cnt, &from, &to);

		// --- Puzzle 1 ---
		for (i = 0; i < cnt; i++) {
			char c = queue_pop(&heads[0][from]);
			queue_add(&heads[0][to], c, true);
		}

		// --- Puzzle 2 ---
		for (i = 0; i < cnt; i++) {
			char c = queue_pop(&heads[1][from]);
			queue_add(&heads[1][0], c, true);
		}
		for (i = 0; i < cnt; i++) {
			char c = queue_pop(&heads[1][0]);
			queue_add(&heads[1][to], c, true);
		}
	READ_BY_LINE_DONE();

	// --- Prints results ---
	printf("1. Crates on the top of each stack: ");
	for (i = 1; i < count; i++) printf("%c", TAILQ_FIRST(&heads[0][i])->c);
	printf("\n");
	printf("2. Crates on the top of each stack: ");
	for (i = 1; i < count; i++) printf("%c", TAILQ_FIRST(&heads[1][i])->c);
	printf("\n");

	// --- Free queues ---
	for (i = 0; i < 2; i++)
		for (j = 0; j < count; j++)
			queue_free(&heads[i][j]);

	return EXIT_SUCCESS;
}
