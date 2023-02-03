#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <regex.h>
#include <file_reader.h>
#include <string_utils.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define MAX_CHILDS 25
#define LIMIT 100000UL
#define DISK_SIZE 70000000UL
#define UPDATE_SIZE 30000000UL

// --- Structs ---
READ_BY_LINE_HEAD(file_t)

struct node {
	struct node* parent;
	char* name;
	unsigned long size;
	unsigned int child_count;
	struct node* childs[MAX_CHILDS];
};

// Creates the root node
struct node* create_root() {
	struct node* n = malloc(sizeof(struct node));
	if (n == NULL) {
		fprintf(stderr, "Malloc failed!");
		exit(EXIT_FAILURE);
	}
	n->parent = NULL;
	n->name = strdup("[root]");
	n->size = 0;
	n->child_count = 0;
	return n;
}

// Adds a child node to another
struct node* add_child(struct node* parent, char* name, unsigned long size) {
	if (parent->child_count == MAX_CHILDS) {
		fprintf(stderr, "Child overflow!");
		exit(EXIT_FAILURE);
	}

	struct node* n = malloc(sizeof(struct node));
	if (n == NULL) {
		fprintf(stderr, "Malloc failed!");
		exit(EXIT_FAILURE);
	}

	n->parent = parent;
	n->name = strdup(name);
	n->size = size;
	n->child_count = 0;

	parent->childs[parent->child_count++] = n;
	return n;
}

// Free the moemory used by a node and all its childrens
void do_free(struct node* n) {
	int i; for (i = 0; i < n->child_count; i++) {
		do_free(n->childs[i]);
		n->childs[i] = NULL;
	}
	if (n->name != NULL) free(n->name);
	free(n);
}

// Private recursive method that prints the tree
void _do_print(struct node* n, int ind) {
	int i; for (i = 0; i < ind; i++) printf("-");
	printf("%s", n->name);
	if (n->size > 0) printf(": %lu", n->size);
	printf("\n");
	for (i = 0; i < n->child_count; i++) _do_print(n->childs[i], ind + 1);
}

// Prints the tree
void print(struct node* n) {
	_do_print(n, 0);
}

// Helper function that selects the smallest number
unsigned long minul(unsigned long a, unsigned long b) {
		if (a < b) return a;
		return b;
}

// Update the tree with the sizes of the directories summing its content
unsigned long update_dir_sizes(struct node* n) {
	if (n->child_count == 0) {
		return n->size;
	}

	unsigned long total = 0;
	int i; for (i = 0; i < n->child_count; i++) total += update_dir_sizes(n->childs[i]);
	n->size = total;
	return total;
}

// Sum used in the puzzle 1
unsigned long sum_puzzle_1(struct node* n) {
	if (n->child_count == 0) {
		return 0;
	}
	unsigned long total = 0;
	int i; for (i = 0; i < n->child_count; i++) total += sum_puzzle_1(n->childs[i]);
	if (n->size <= LIMIT) total += n->size;
	return total;
}

// Search used in the puzzle 2
unsigned long find_puzzle_2(struct node* n, unsigned long need) {
	if (n->size < need || n->child_count == 0) {
		return ULONG_MAX;
	}
	unsigned long min = ULONG_MAX;
	int i; for (i = 0; i < n->child_count; i++) {
		min = minul(min, find_puzzle_2(n->childs[i], need));
	}
	return  minul(min, n->size);
}

// --- Main function ---
int main(void) {
	// --- Regext ---
	regex_t regex;
	int rc = regcomp(&regex, "^[0-9]+.*$", REG_EXTENDED);
	if (rc != 0) {
		fprintf(stderr, "Could not compile regex");
		exit(EXIT_FAILURE);
	}

	// --- Variables ---
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	struct node* root = create_root();
	struct node* ptr = root;
	char line[file.max];
	char buffer[file.max];

	// --- Reads and parse the input file ---
	READ_BY_LINE_WHILE(file, line)
		if (strcmp(line, "$ cd /") == 0) continue;
		if (starts_with("$ cd ", line)) {
			sscanf(line, "$ cd %s", buffer);
			if (strcmp(buffer, "..") == 0) {
				ptr = ptr->parent;
			} else {
				ptr = add_child(ptr, buffer, 0);
			}
		} else if (regexec(&regex, line, 0, NULL, 0) == 0) {
			int size;
			sscanf(line, "%d %s", &size, buffer);
			add_child(ptr, buffer, size);
		}
	READ_BY_LINE_DONE(file)
	regfree(&regex);

	// --- Update the tree dir sizes ---
	update_dir_sizes(root);

	// --- Puzzle 1 ---
	unsigned long total = sum_puzzle_1(root);
	printf("1. The sum of the total sizes of the directories under %lu is %lu\n", LIMIT, total);

	// --- Puzzle 2 ---
	unsigned long need = UPDATE_SIZE - DISK_SIZE + root->size;
	total = find_puzzle_2(root, need);
	printf("2. The total size of the smalles directory needed is %lu\n", total);

	// --- Free resources ---
	do_free(root);
	ptr = NULL;
	root = NULL;

	return EXIT_SUCCESS;
}
