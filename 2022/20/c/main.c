#include <stdio.h>
#include <stdlib.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define KEY 811589153L
#define MIX_COUNT 10
#define OFFSET 1000

// --- Structs ---
typedef struct Node {
	long value;
	struct Node* prev;
	struct Node* next;
} Node;

// --- Debug and test functions ---
void check_chain(Node* root, int len) {
	Node* ptr = root;
	int i; for (i = 0; i < len; i++) {
		if (ptr->next == NULL) fprintf(stderr, "Missing next pointer\n");
		if (ptr->prev == NULL) fprintf(stderr, "Missing prev pointer\n");
		if (ptr->next->prev != ptr) fprintf(stderr, "Illegal prev pointer\n");
		if (ptr->prev->next != ptr) fprintf(stderr, "Illegal next pointer\n");
		ptr = ptr->next;
	}
	if (ptr != root) fprintf(stderr, "Missing circle pointer\n");
}

void print_chain(Node* root) {
	Node* ptr = root;
	printf("[ ");
	while (1) {
		printf("%ld ", ptr->value);
		ptr = ptr->next;
		if (ptr == root) break;
	}
	printf("]\n");
}

// --- Functions ---
void mix(Node* order[], int len) {
	Node *org, *dst;
	int i, j, n;
	for (i = 0; i < len; i++) {
		org = order[i];
		dst = org;
		n = org->value % (len - 1);
		if (n == 0) continue;
		if (n > 0) for (j = 0; j < n; j++) dst = dst->next;
		else for (j = 0; j <= -1 *n; j++) dst = dst->prev;
		org->prev->next = org->next;
		org->next->prev = org->prev;
		org->prev = dst;
		org->next = dst->next;
		org->next->prev = org;
		dst->next = org;
	}
}

long sum(Node* head, int len) {
	int i, j;
	long offset = OFFSET % len;
	long total = 0;
	while (head->value != 0) head = head->next;
	for (i = 0; i < 3; i++) {
		for (j = 0; j < offset; j++) {
			head = head->next;
		}
		total += head->value;
	}
	return total;
}

// --- Main function ---
int main(void) {
	// --- Variables ---
	READ_BY_LINE_HEAD(file_t);
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	const int len = file.rows;
	Node* order[len];
	long value;
	int i = 0;
	char line[file.max];

	// --- Read and parse the input file ---
	READ_BY_LINE_WHILE(file, line)
		sscanf(line, "%ld", &value);
		Node* node = malloc(sizeof(Node));
		node->value = value;
		order[i] = node;
		if (i > 0) {
			order[i]->prev = order[i - 1];
			order[i - 1]->next = order[i];
		}
		if (i == len - 1) {
			order[0]->prev = order[i];
			order[i]->next = order[0];
		}
		i++;
	READ_BY_LINE_DONE(file);

	// --- Puzzle 1 ---
	mix(order, len);
	long total = sum(order[0], len);
	printf("1. The sum of the coordinates is: %ld\n", total);

	// --- Puzzle 2 ---
	for (i = 0; i < len; i++) {
		order[i]->value = order[i]->value * KEY;
		if (i > 0) order[i]->prev = order[i - 1];
		if (i < len - 1) order[i]->next = order[i + 1];
	}
	order[0]->prev = order[len - 1];
	order[len - 1]->next = order[0];
	for (i = 0; i < MIX_COUNT; i++) {
		mix(order, len);
	}
	total = sum(order[0], len);
	printf("2. The sum of the coordinates is: %ld\n", total);


	// --- Free resources ---
	for (i = 0; i < len; i++) free(order[i]);

	return EXIT_SUCCESS;
}
