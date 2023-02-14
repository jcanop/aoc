#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <lib.h>

// --- Constants ---
const Point SHAPE_0[4] = {{.x = 0, .y = 0}, {.x = 1, .y = 0}, {.x = 2, .y = 0}, {.x = 3, .y = 0}};
const Point SHAPE_1[5] = {{.x = 1, .y = 0}, {.x = 0, .y = 1}, {.x = 1, .y = 1}, {.x = 2, .y = 1}, {.x = 1, .y = 2}};
const Point SHAPE_2[5] = {{.x = 2, .y = 0}, {.x = 2, .y = 1}, {.x = 0, .y = 2}, {.x = 1, .y = 2}, {.x = 2, .y = 2}};
const Point SHAPE_3[4] = {{.x = 0, .y = 0}, {.x = 0, .y = 1}, {.x = 0, .y = 2}, {.x = 0, .y = 3}};
const Point SHAPE_4[4] = {{.x = 0, .y = 0}, {.x = 1, .y = 0}, {.x = 0, .y = 1}, {.x = 1, .y = 1}};

// --- Functions ---
Row* row_create_node(void) {
	Row* ptr = malloc(sizeof(Row));
	int i; for (i = 0; i < WIDTH; i++) ptr->data[i] = AIR;
	ptr->prev = NULL;
	ptr->next = NULL;
	return ptr;
}

void chamber_add_row(Chamber* c) {
	Row* row = row_create_node();
	c->root->prev = row;
	row->next = c->root;
	c->root = row;
}

Row* chamber_find_from_top(Chamber* c, char tile) {
	int i;
	Row* ptr = c->root;
	while (ptr != NULL) {
		for (i = 0; i < WIDTH; i++) if (ptr->data[i] == tile) return ptr;
		ptr = ptr->next;
	}
	return NULL;
}

void chamber_add_shape(Chamber* c, int shape_id) {
	const Point* p;
	int len = 0;
	switch (shape_id) {
		case 0: p = &SHAPE_0[0]; len = 4; c->shape_len = 1; break;
		case 1: p = &SHAPE_1[0]; len = 5; c->shape_len = 3; break;
		case 2: p = &SHAPE_2[0]; len = 5; c->shape_len = 3; break;
		case 3: p = &SHAPE_3[0]; len = 4; c->shape_len = 4; break;
		case 4: p = &SHAPE_4[0]; len = 4; c->shape_len = 2; break;
		default: c->shape_len = 0;
	}
	Row* ptr = chamber_find_from_top(c, SAND);
	ptr = (ptr == NULL) ? c->root : ptr->prev;
	int i, j;
	for (i = 1; i < MARGIN_TOP + c->shape_len; i++) {
		if (ptr->prev == NULL) {
			chamber_add_row(c);
		}
		ptr = ptr->prev;
	}
	for (i = 0; i < c->shape_len; i++) {
		for (j = 0; j < len; j++) {
			if (p[j].y == i) ptr->data[p[j].x + MARGIN_LEFT] = ROCK;
		}
		ptr = ptr->next;
	}
}

bool chamber_can_move(Chamber* c, char direction) {
	Row *ptr, *up;
	int i, j;
	if (direction == DOWN) {
		Row* ptr = chamber_find_from_top(c, ROCK);
		for (i = 1; i < c->shape_len; i++) ptr = ptr->next;
		if (ptr->next == NULL) return false;
		ptr = ptr->next;
		for (i = 0; i < c->shape_len; i++) {
			up = ptr->prev;
			for (j = 0; j < WIDTH; j++) {
				if (up->data[j] == ROCK && ptr->data[j] == SAND) return false;
			}
			ptr = ptr->prev;
		}
	} else if (direction == LEFT) {
		Row* ptr = chamber_find_from_top(c, ROCK);
		for (i = 0; i < c->shape_len; i++) {
			if (ptr->data[0] == ROCK) return false;
			for (j = 1; j < WIDTH; j++) {
				if (ptr->data[j] == ROCK) {
					if (ptr->data[j - 1] != AIR) return false;
					break;
				}
			}
			ptr = ptr->next;
		}
	} else if (direction == RIGHT) {
		Row* ptr = chamber_find_from_top(c, ROCK);
		for (i = 0; i < c->shape_len; i++) {
			if (ptr->data[WIDTH - 1] == ROCK) return false;
			for (j = WIDTH - 2; j >= 0; j--) {
				if (ptr->data[j] == ROCK) {
					if (ptr->data[j + 1] != AIR) return false;
					break;
				}
			}
			ptr = ptr->next;
		}
	}

	return true;
}

bool chamber_move(Chamber* c, char direction) {
	int i, j;
	if (chamber_can_move(c, direction)) {
		if (direction == DOWN) {
			Row *up = NULL, *ptr = chamber_find_from_top(c, ROCK);
			for (i = 0; i < c->shape_len; i++) ptr = ptr->next;
			for (i = 0; i < c->shape_len; i++) {
				up = ptr->prev;
				for (j = 0; j < WIDTH; j++) {
					if (up->data[j] == ROCK) {
						ptr->data[j] = up->data[j];
						up->data[j] = AIR;
					}
				}
				ptr = ptr->prev;
			}
		} else if (direction == LEFT) {
			Row* ptr = chamber_find_from_top(c, ROCK);
			for (i = 0; i < c->shape_len; i++) {
				for (j = 0; j < WIDTH - 1; j++) {
					if (ptr->data[j + 1] == ROCK) {
						ptr->data[j] = ptr->data[j + 1];
						ptr->data[j + 1] = AIR;
					}
				}
				ptr = ptr->next;
			}
		} else if (direction == RIGHT) {
			Row* ptr = chamber_find_from_top(c, ROCK);
			for (i = 0; i < c->shape_len; i++) {
				for (j = WIDTH; j > 0 ; j--) {
					if (ptr->data[j - 1] == ROCK) {
						ptr->data[j] = ptr->data[j - 1];
						ptr->data[j - 1] = AIR;
					}
				}
				ptr = ptr->next;
			}
		}
	} else if (direction == DOWN) {
		Row *ptr = chamber_find_from_top(c, ROCK);
		for (i = 0; i < c->shape_len; i++) {
			for (j = 0; j < WIDTH; j++) {
				if (ptr->data[j] == ROCK) ptr->data[j] = SAND;
			}
			ptr = ptr->next;
		}
		return false;
	}

	return true;
}

long chamber_height(Chamber* c) {
	long total = 0;
	Row* ptr = chamber_find_from_top(c, SAND);
	while (ptr->next != NULL) {
		total++;
		ptr = ptr->next;
	}
	return total - 1;
}

void chamber_print(Chamber* c) {
	int i;
	Row* ptr = c->root;
	while (ptr != NULL) {
		printf("|");
		for (i = 0; i < WIDTH; i++) printf("%c", ptr->data[i]);
		printf("|\n");
		ptr = ptr->next;
	}
	printf("+");
	for (i = 0; i < WIDTH; i++) printf("-");
	printf("+\n");
}

void chamber_free(Chamber* c) {
	Row* ptr = c->root;
	while (ptr != NULL) {
		Row* row = ptr;
		ptr = ptr->next;
		free(row);
	}
}


