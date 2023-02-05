#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <file_reader.h>
#include <set.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define ROPE_LEN_1 2
#define ROPE_LEN_2 10
#define SIZE sizeof(int)

// --- Macros ---
#define knot_hash(knot) {\
	memcpy(&buffer[0], &knot->x, SIZE);\
	memcpy(&buffer[SIZE], &knot->y, SIZE);\
}

// --- Structs ---
struct Knot {
	int x;
	int y;
};

struct Node {
	size_t child_count;
	struct Node* childs[256];
};

char buffer[SIZE * 2];

// --- Util functions ---
int abs(int x) {
	if (x < 0) return -1 * x;
	return x;
}

int signum(int x) {
	if (x < 0) return -1;
	if (x > 0) return 1;
	return 0;
}

// --- Tree functions ---
struct Node* tree_create_node() {
	struct Node* node = malloc(sizeof(struct Node));
	node->child_count = 0;
	int i; for (i = 0; i < 256; i++) node->childs[i] = NULL;
	return node;
}

void tree_add_knot(struct Node* node, struct Knot* knot) {
	knot_hash(knot);
	struct Node* ptr = node;
	int i; for (i = 0; i < SIZE * 2; i++) {
		unsigned char idx = buffer[i] & 0xFF;
		if (ptr->childs[idx] == NULL) {
			ptr->childs[idx] = tree_create_node();
			ptr->child_count++;
		}
		ptr = ptr->childs[idx];
	}
}

/*
bool tree_contains_knot(struct Node* node, struct Knot knot) {
	knot_hash(knot);
	struct Node* ptr = node;
	for (int i = 0; i < SIZE * 2; i++) {
		size_t idx = buffer[i] & 0xFF;
		if (ptr->childs[idx] == NULL) return false;
		ptr = ptr->childs[idx];
	}
	return true;
}
*/

size_t tree_count(struct Node* node) {
	if (node->child_count == 0) return 1;

	size_t total = 0;
	int i; for (i = 0; i < 256; i++) {
		if (node->childs[i] != NULL) total += tree_count(node->childs[i]);
	}
	return total;
}

void tree_free(struct Node* node) {
	for (int i = 0; i < 256; i++) {
		if (node->childs[i] != NULL) {
			tree_free(node->childs[i]);
			node->childs[i] = NULL;
		}
	}
	free(node);
}

// --- Rope function ---
void move(struct Knot* head, size_t len, char direction, struct Node* tree) {
	switch(direction) {
		case 'U': head->y--; break;
		case 'D': head->y++; break;
		case 'L': head->x--; break;
		case 'R': head->x++; break;
		default:
			fprintf(stderr, "Unsupported direction: %c\n", direction);
			exit(EXIT_FAILURE);
	}

	for (int i = 1; i < len; i++) {
		struct Knot* c = head + i - 1;
		struct Knot* n = head + i;
		int dx = c->x - n->x;
		int dy = c->y - n->y;
		if (abs(dx) < 2 && abs(dy) < 2) break;
		n->x += signum(dx);
		n->y += signum(dy);
	}

	tree_add_knot(tree, head + len - 1);
}

// --- Main function ---
int main(void) {
	// --- Variables ---
	struct Node* root1 = tree_create_node();
	struct Node* root2 = tree_create_node();

	// --- Ropes ---
	int i;
	struct Knot rope1[ROPE_LEN_1];
	for (i = 0; i < ROPE_LEN_1; i++) { rope1[i].x = 0; rope1[i].y = 0; }
	struct Knot rope2[ROPE_LEN_2];
	for (i = 0; i < ROPE_LEN_2; i++) { rope2[i].x = 0; rope2[i].y = 0; }

	// --- Read and parse the input file ---
	READ_BY_LINE_HEAD(file_t)
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	char line[file.max];
	READ_BY_LINE_WHILE(file, line)
		char d; int n;
		sscanf(line, "%c %d", &d, &n);
		for (i = 0; i < n; i++) {
			move(&rope1[0], ROPE_LEN_1, d, root1);
			move(&rope2[0], ROPE_LEN_2, d, root2);
		}
	READ_BY_LINE_DONE(file)

	printf("1. Positions visited at least once: %zu\n", tree_count(root1));
	printf("2. Positions visited at least once: %zu\n", tree_count(root2));

	// --- Free resources ---
	tree_free(root1); root1 = NULL;
	tree_free(root2); root2 = NULL;

	return EXIT_SUCCESS;
}
