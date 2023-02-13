#ifndef SET_H_
#define SET_H_

#include <stdlib.h>
#include <stdbool.h>

// --- Constants ---
#define ID_LEN 2
#define OFFSET 'A'
#define SET_MAX 'Z' - 'A' + 1

// --- Structs ---
typedef struct SetNode {
	int value;
	bool is_leaf;
	struct SetNode* childs[SET_MAX];
} SetNode;

// Creates a new set
SetNode* set_create();

// Set a value
void set_put(SetNode* root, const char* id);

// Set a flow value
void set_put_flow(SetNode* root, const char* id, int flow);

// Get a flow value
int* set_get_flow(SetNode* root, const char* id);

// Gets the total flow
int set_total_flow(SetNode* root);

// Set a distance value
void set_put_distance(SetNode* root, const char* from, const char* to, int distance);

// Get a distance value
int* set_get_distance(SetNode* root, const char* from, const char* to);

// Checks if entry exists
bool set_contains(SetNode* root, const char* id);

// Remove a entry
void set_remove(SetNode* root, const char* id);

// Clone a set
void set_clone(SetNode* dst, const SetNode* src);

// Gets a set with the elements of s1 if are not present in s2
void set_difference(SetNode* dst, const SetNode* s1, const SetNode* s2);

// Free the allocated memory
void set_free(SetNode* root);

#endif /* SET_H_ */
