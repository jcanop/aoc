#ifndef MAP_H_
#define MAP_H_

// --- Constants ---
#define EMPTY  ' '
#define OPEN   '.'
#define WALL   '#'
#define RIGHT  'R'
#define LEFT   'L'
#define EAST   0
#define SOUTH  1
#define WEST   2
#define NORTH  3
#define M_SIZE 50
#define M_HEIGHT (M_SIZE * 4)
#define M_WIDTH (M_SIZE * 3)

// --- Structs ---
typedef struct Map {
	char grid[M_HEIGHT][M_WIDTH];
	int x;
	int y;
	int d;
	char* cmds;
} Map;

typedef struct Tokenizer {
	char *ptr;
	int idx;
} Tokenizer;

// --- Functions ---
int map_load(Map* m, char* filename);

void map_path(Map* m, void (*f)(Map*, int));

long map_password(Map* m);

void map_free(Map* m);

#endif /* MAP_H_ */
