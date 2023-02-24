#ifndef MAP_H_
#define MAP_H_

#include <lib.h>
#include <queue.h>

// --- Structs ---
typedef TAILQ_HEAD(ElfList, Elf) ElfList;

typedef struct Map {
	ElfList* list;
	Index* index;
	Index* prop;
	int count;
	int round;
} Map;

// --- Functions ---
Map* map_create(void);

void map_add_elf(Map* map, int x, int y);

bool map_round(Map* map);

int map_empty_count(Map* map);

void map_print(Map* map);

void map_free(Map* map);

#endif /* MAP_H_ */
