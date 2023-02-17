#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <lib.h>

// Creates a state
State* state_create() {
	State* e = malloc(sizeof(State));
	if (e == NULL) {
		fprintf(stderr, "Malloc failed!");
		exit(EXIT_FAILURE);
	}
	e->data[0] = 1;
	int i; for (i = 1; i < STATE_LEN; i++) e->data[i] = 0;
	e->next = NULL;
	return e;
}

// Clones a state with the bit mask blank
State* state_clone(const State* s) {
	State* e = state_create();
	int i; for (i = 0; i < STATE_LEN; i++) e->data[i] = s->data[i];
	e->data[MASK] = 0;
	return e;
}

// Mine
void state_mine(State* s) {
	int i; for (i = 0; i < 4; i++) s->data[M + i] += s->data[i];
	s->data[TIME]++;
}

// Should I build a kind of robot?
bool state_should_build(const State* s, const Blueprint* bp, int robot) {
	// --- check if we hae enough meterials ---
	int i; for (i = 0; i < 3; i++) if (s->data[M + i] < bp->costs[robot][i]) return false;

	// --- Always build a Geode roboto if we can ---
	if (robot == GEODE) return true;

	// --- Check for bit mask ---
	int m = (1 << robot);
	if ((s->data[MASK] & m) == m) return false;

	// --- We shouldn't build more robots that needed ---
	switch(robot) {
		case ORE:      return s->data[robot] < bp->max_ore;
		case CLAY:     return s->data[robot] < bp->costs[OBSIDIAN][CLAY];
		case OBSIDIAN: return s->data[robot] < bp->costs[GEODE][OBSIDIAN];
		default: fprintf(stderr, "Unsupported argument: %d\n", robot); exit(EXIT_FAILURE);
	}
}

// Build a robot
void state_build(State* s, const Blueprint* bp, int robot) {
	int i; for (i = 0; i < 3; i++) s->data[M + i] -= bp->costs[robot][i];
	s->data[robot]++;
}

// Prints a state into the console
void state_print(State* s) {
	printf("[ ");
	int i; for (i = 0; i < STATE_LEN; i++) printf("%d ", s->data[i]);
	printf("]\n");
}

// Creates a queue loaded with the initial state
Queue* queue_create(void) {
	Queue* q = malloc(sizeof(Queue));
	q->head = state_create();
	q->tail = q->head;
	return q;
}

// Pop a state from the queue
State* queue_pop(Queue* q) {
	if (q->head == NULL) return NULL;
	State* s = q->head;
	q->head = q->head->next;
	s->next = NULL;
	return s;
}

// Push a state at the bottom of the queue
void queue_push(Queue* q, State* s) {
	if (q->head == NULL) {
		q->head = s;
		q->tail = q->head;
	} else {
		q->tail->next = s;
		q->tail = q->tail->next;
	}
}

// Checks if a queue is empty
bool queue_empty(Queue* q) {
	return (q->head == NULL);
}

// Prints a Blueprint in the console
void blueprint_print(Blueprint* bp) {
	printf("[Blueprint %d] costs: ", bp->id);
	int i; for (i = 0; i < 4; i++) {
		printf("[ ");
		int j; for (j = 0; j < 3; j++) {
			printf("%d ", bp->costs[i][j]);
		}
		printf("]");
	}
	printf(", max_ore: %d\n", bp->max_ore);
}

// Get the max number of geodes produced for a blueprint
int get_max(const Blueprint* bp, int limit) {
	Queue* queue = queue_create();
	State* s = NULL;
	int max = 0, i;

	while(!queue_empty(queue)) {
		s = queue_pop(queue);
		if (s == NULL) {
			fprintf(stderr, "Null pointer exception!");
			exit(EXIT_FAILURE);
		}

		if (s->data[TIME] == limit) {
			if (max < s->data[M + GEODE]) max = s->data[M + GEODE];
			free(s); s = NULL;
			continue;
		}

		if (state_should_build(s, bp, GEODE)) {
			state_mine(s);
			state_build(s, bp, GEODE);
			queue_push(queue, s);
			continue;
		}

		for (i = 2; i >= 0; i--) {
			if (state_should_build(s, bp, i)) {
				State* clone = state_clone(s);
				state_mine(clone);
				state_build(clone, bp, i);
				queue_push(queue, clone);

				s->data[MASK] |= (1 << i);
			}
		}

		state_mine(s);
		queue_push(queue, s);
	}

	free(queue);
	return max;
}
