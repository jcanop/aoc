#include <limits.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <lib.h>

// Creates a new index
Index* index_create(void) {
	Index* idx = malloc(sizeof(Index));
	int i;
	for (i = 0; i < INT_SIZE; i++) idx->buffer[i] = 0;
	for (i = 0; i < 256; i++) idx->next[i] = NULL;
	idx->min_x = INT_MAX; idx->max_x = INT_MIN;
	idx->min_y = INT_MAX; idx->max_y = INT_MIN;
	return idx;
}

// creates a new index node
IndexNode* index_create_node(void) {
	IndexNode* n = malloc(sizeof(IndexNode));
	n->elf = NULL;
	int i; for (i = 0; i < 256; i++) n->next[i] = NULL;
	return n;
}

// Index an Elf, returns false if there is a collision
bool index_add(Index* idx, Elf* e, int x, int y) {
	INDEX_HASH(idx, x, y);
	unsigned char c = idx->buffer[0] & 0xFF;
	if (idx->next[c] == NULL) {
		idx->next[c] = index_create_node();
	}
	IndexNode* ptr = idx->next[c];

	int i; for (i = 1; i < INT_SIZE * 2; i++) {
		c = idx->buffer[i] & 0xFF;
		if (ptr->next[c] == NULL) {
			ptr->next[c] = index_create_node();
		}
		ptr = ptr->next[c];
	}
	if (ptr->elf != NULL) return false;
	ptr->elf = e;
	if (x < idx->min_x) idx->min_x = x;
	if (x > idx->max_x) idx->max_x = x;
	if (y < idx->min_y) idx->min_y = y;
	if (y > idx->max_y) idx->max_y = y;
	return true;
}

// Gets the indexed Elf
Elf* index_get(Index* idx, int x, int y) {
	INDEX_HASH(idx, x, y);
	unsigned char c = idx->buffer[0] & 0xFF;
	if (idx->next[c] == NULL) return NULL;
	IndexNode* ptr = idx->next[c];

	int i; for (i = 1; i < INT_SIZE * 2; i++) {
		c = idx->buffer[i] & 0xFF;
		if (ptr->next[c] == NULL) return NULL;
		ptr = ptr->next[c];
	}
	if (ptr->elf == NULL) return NULL;
	return ptr->elf;
}

// Checks if an index has an elemnt at x, y position
bool index_has(Index* idx, int x, int y) {
	return index_get(idx, x, y) != NULL;
}

// Recursive function for clear the index
void index_clear_node(IndexNode* node) {
	int i; for (i = 0; i < 256; i++) {
		if (node->next[i] != NULL) index_clear_node(node->next[i]);
	}
	node->elf = NULL;
}

// Clears an index
void index_clear(Index* idx) {
	int i; for (i = 0; i < 256; i++) {
		if (idx->next[i] != NULL) index_clear_node(idx->next[i]);
	}
	idx->min_x = INT_MAX; idx->max_x = INT_MIN;
	idx->min_y = INT_MAX; idx->max_y = INT_MIN;
}

// Recursive function for free memory
void index_free_node(IndexNode* node) {
	int i; for (i = 0; i < 256; i++) {
		if (node->next[i] != NULL) index_free_node(node->next[i]);
	}
	free(node);
}

// Free the resources
void index_free(Index* idx) {
	int i; for (i = 0; i < 256; i++) {
		if (idx->next[i] != NULL) index_free_node(idx->next[i]);
	}
	free(idx);
}

// Prints an elf into the console
void elf_print(Elf* e) {
	if (e == NULL) printf("NULL\n");
	else printf("Elf: (%d, %d), proposed: (%d, %d)\n", e->x, e->y, e->px, e->py);
}
