#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <queue.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"

// --- Structs ---
struct node { int x; int y; TAILQ_ENTRY(node) nodes; };
TAILQ_HEAD(head_s, node) head;
READ_BY_LINE_HEAD(file_t)

// --- Macros ---
#define map(y, x) map[(y) * w + (x)]

// Adds a new point into the queue
void queue_add(struct head_s* head, int x, int y) {
	struct node* e = malloc(sizeof(struct node));
	if (e == NULL) {
		fprintf(stderr, "Malloc failed");
		exit(EXIT_FAILURE);
	}
	e->x = x; e->y = y;
	TAILQ_INSERT_TAIL(head, e, nodes);
	e = NULL;
}

// Find path between two points
int find_path(const char* map, int w, int h, int start, int end) {
	int a[h][w];
	int x, y;
	for (y = 0; y < h; y++) for (x = 0; x < w; x++) a[y][x] = 0;

	x = start % w; y = start / w;
	a[y][x] = 1;

	TAILQ_INIT(&head);
	queue_add(&head, x, y);
	while(!TAILQ_EMPTY(&head)) {
		struct node* e = TAILQ_FIRST(&head);
		TAILQ_REMOVE(&head, e, nodes);
		x = e->x; y = e->y;
		free(e);
		e = NULL;

		if (x > 0     && a[y][x - 1] == 0 && map(y, x - 1) <= map(y, x) + 1) { a[y][x - 1] = a[y][x] + 1; queue_add(&head, x - 1, y); }
		if (y > 0     && a[y - 1][x] == 0 && map(y - 1, x) <= map(y, x) + 1) { a[y - 1][x] = a[y][x] + 1; queue_add(&head, x, y - 1); }
		if (x < w - 1 && a[y][x + 1] == 0 && map(y, x + 1) <= map(y, x) + 1) { a[y][x + 1] = a[y][x] + 1; queue_add(&head, x + 1, y); }
		if (y < h - 1 && a[y + 1][x] == 0 && map(y + 1, x) <= map(y, x) + 1) { a[y + 1][x] = a[y][x] + 1; queue_add(&head, x, y + 1); }
	}

	x = end % w; y = end / w;
	return a[y][x] - 1;
}

// Find the shortest path to the end point
int find_shortest_path(const char* map, int w, int h, int end) {
	int min = INT_MAX;
	int x, y;
	for (y = 0; y < h; y++) {
		for (x = 0; x < w; x++) {
			if (map(y, x) == 'a') {
				int start = y * w + x;
				int count = find_path(map, w, h, start, end);
				if (count > 0 && count < min) min = count;
			}
		}
	}
	return min;
}

// --- Main function ---
int main(void) {
	// --- Variables ---
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	char* map;
	int width = file.max - 2;
	int height = file.rows;

	// --- Read and parse the input file ---
	READ_BY_LINE_FULL(file, map)

	// --- Search start and end points ---
	char* ptr = strchr(map, 'S');
	if (ptr == NULL) {
		fprintf(stderr, "Start point not found!\n");
		exit(EXIT_FAILURE);
	}
	*ptr = 'a';
	int start = ptr - map;

	ptr = strchr(map, 'E');
	if (ptr == NULL) {
		fprintf(stderr, "End point not found!\n");
		exit(EXIT_FAILURE);
	}
	*ptr = 'z';
	int end = ptr - map;

	// --- Puzzle 1 ---
	int count = find_path(map, width, height, start, end);
	printf("1. Fewest steps: %d\n", count);

	// --- Puzzle 2 ---
	int min = find_shortest_path(map, width, height, end);
	printf("2. Shortest path: %d\n", min);

	// --- Free resources ---
	free(map); map = NULL;

	return EXIT_SUCCESS;
}
