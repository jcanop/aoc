#ifndef LIB_H_
#define LIB_H_

#include <stdlib.h>
#include <stdbool.h>
#include <queue.h>

// --- Constants ---
#define INT_SIZE sizeof(int)

// --- Macros ---
#define INDEX_HASH(root, x, y) \
	memcpy(&root->buffer[0],        &x, INT_SIZE); \
	memcpy(&root->buffer[INT_SIZE], &y, INT_SIZE);

// --- Structs ---
typedef struct Elf {
	int x, y;
	int px, py;
	TAILQ_ENTRY(Elf) nodes;
} Elf;

typedef struct IndexNode {
	Elf* elf;
	struct IndexNode* next[256];
} IndexNode;

typedef struct Index {
	char buffer[INT_SIZE * 2];
	IndexNode* next[256];
	int min_x, max_x, min_y, max_y;
} Index;

// --- Functions ---
Index* index_create(void);

bool index_add(Index* idx, Elf* e, int x, int y);

Elf* index_get(Index* idx, int x, int y);

bool index_has(Index* idx, int x, int y);

void index_clear(Index* idx);

void index_free(Index* idx);

void elf_print(Elf* e);

#endif /* LIB_H_ */
