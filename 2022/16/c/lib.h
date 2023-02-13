#ifndef LIB_H_
#define LIB_H_

#include <uthash.h>
#include <set.h>

// --- Constants ---
#define ID_SIZE 3
#define MAX_TO 6

// ---- Structs ---
typedef struct Tunnels {
	char id[ID_SIZE];
	char leads[MAX_TO][ID_SIZE];
	UT_hash_handle hh;
} Tunnels;

typedef struct Layout {
	SetNode* flows;
	SetNode* distances;
	char (*useful)[ID_SIZE];
	int useful_len;
} Layout;

// Free memory used by a layout struct
void layout_free(Layout* layout);

// Floyd-Warshall Algorithm
SetNode* floyd_warshal(char valves[][ID_SIZE], int count, Tunnels* tunnels);

// Depth First Search: Puzzle 1
int dfs1 (Layout* layout);

// Depth First Search: Puzzle 2
int dfs2(Layout* layout);

#endif /* LIB_H_ */
