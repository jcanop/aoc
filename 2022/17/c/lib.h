#ifndef LIB_H_
#define LIB_H_

#include <stdbool.h>

// --- Definitions ---
#define WIDTH 7
#define MARGIN_LEFT 2
#define MARGIN_TOP 3
#define SHAPE_LEN 5
#define AIR '.'
#define ROCK '@'
#define SAND '#'
#define LEFT '<'
#define RIGHT '>'
#define DOWN 'v'

// --- Structs ---
typedef struct Point {
	int x;
	int y;
} Point;

typedef struct Row {
	char data[WIDTH];
	struct Row* prev;
	struct Row* next;
} Row;

typedef struct Chamber {
	Row* root;
	unsigned int shape_len;
} Chamber;

// Creates a new row node
Row* row_create_node(void);

// Adds a row at the top of the chamber
void chamber_add_row(Chamber* c);

// Adds a shape into the chamber
void chamber_add_shape(Chamber* c, int shape_id);

// Move the shape inside the chamber
bool chamber_move(Chamber* c, char direction);

// Calcualtes the tower height
long chamber_height(Chamber* c);

// Prints the chamber
void chamber_print(Chamber* c);

// Frees the resources used by the chamber
void chamber_free(Chamber* c);

#endif /* LIB_H_ */
