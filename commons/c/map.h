#ifndef MAP_H_
#define MAP_H_

#include <stdlib.h>
#include <string.h>

// --- Constants ---
#define INT_SIZE sizeof(int)

// --- Macros ---
#define MAP_HASH_XY(x, y)\
	memcpy(&root->buffer[0],        &x, INT_SIZE);\
	memcpy(&root->buffer[INT_SIZE], &y, INT_SIZE);

// --- Structs ---
struct MapNodeRoot {
	char buffer[INT_SIZE * 2];
	struct MapNode* childs[256];
	int min_x, max_x, min_y, max_y;
};

struct MapNode {
	char c;
	struct MapNode* childs[256];
};

// Creates a new map
struct MapNodeRoot* map_create(void);

// Puts a value into the map
void map_put(struct MapNodeRoot* root, int x, int y, char c);

// Gets the value fromthe map
char* map_get(struct MapNodeRoot* root, int x, int y);

// Free the map resources
void map_free(struct MapNodeRoot* root);

#endif /* MAP_H_ */
