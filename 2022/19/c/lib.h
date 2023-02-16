#ifndef LIB_H_
#define LIB_H_

// --- Constants ---
#define ORE        0
#define CLAY       1
#define OBSIDIAN   2
#define GEODE      3
#define M          4
#define MASK       8
#define TIME       9
#define STATE_LEN 10

// --- Structs ---
typedef struct Blueprint {
	int id;
	int costs[4][3];
	int max_ore;
} Blueprint;

typedef struct State {
	int data[STATE_LEN];
	struct State* next;
} State;

typedef struct Queue {
	State* head;
	State* tail;
} Queue;

// Get the max number of geodes produced for a blueprint
int get_max(const Blueprint* bp, int limit);

#endif /* LIB_H_ */
