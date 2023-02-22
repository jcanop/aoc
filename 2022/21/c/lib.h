#ifndef LIB_H_
#define LIB_H_

#include <queue.h>
#include <uthash.h>

// --- Constants ---
#define KEY_SIZE 5

// --- Structs ---
typedef struct Operation {
	char key[KEY_SIZE];
	char op1[KEY_SIZE];
	char op2[KEY_SIZE];
	char operator;
	TAILQ_ENTRY(Operation) nodes;
} Operation;

typedef struct Solution {
	char id[KEY_SIZE];
	long value;
	UT_hash_handle hh;
} Solution;

TAILQ_HEAD(OperationHead, Operation);

typedef struct Resolver {
	struct OperationHead queue;
	struct Solution* solutions;
} Resolver;

// --- Functions ---
void resolver_init(Resolver* resolver);

int resolver_load(Resolver* resolver, char* filename);

void resolver_update_for_puzzle2(Resolver* resolver, char* root, char* me);

long resolver_solve_for(Resolver* resolver, char* id);

void resolver_free(Resolver* resolver);

#endif /* LIB_H_ */
