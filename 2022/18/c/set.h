#ifndef SET_H_
#define SET_H_

#include <stdbool.h>

#define INT_SIZE sizeof(int)

typedef struct SetNode {
	struct SetNode* next[256];
} SetNode;

typedef struct Set {
	unsigned char id[INT_SIZE * 3];
	SetNode* root;
} Set;

typedef unsigned int uint;

// Creates a new set
Set* set_create();

// Adds a point into the set
void set_add(Set* set, uint x, uint y, uint z);

// Add all points from other set into this
void set_add_all(Set* dst, Set* src);

// Checks if a point is in the set
bool set_contains(Set* set, uint x, uint y, uint z);

// Execute a function on each element in the set
void set_do(Set* set, void (*f)(uint x, uint y, uint z));

// Free the set resources
void set_free(Set* set);

#endif /* SET_H_ */
