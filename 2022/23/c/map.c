#include <stdio.h>
#include <map.h>

// Creates a map
Map* map_create(void) {
	Map* map = malloc(sizeof(Map));
	map->list = malloc(sizeof(ElfList));
	TAILQ_INIT(map->list);
	map->index = index_create();
	map->prop = index_create();
	map->count = 0;
	map->round = 0;
	return map;
}

// Adds an elf into the map at a position
void map_add_elf(Map* map, int x, int y) {
	Elf* elf = malloc(sizeof(Elf));
	elf->x = x; elf->y = y;
	elf->px = x; elf->py = y;
	TAILQ_INSERT_TAIL(map->list, elf, nodes);
	index_add(map->index, elf, x, y);
	map->count++;
}

// Simulates a round
bool map_round(Map* map) {
	int i, x, y, n;
	bool found = false;
	bool result = false;

	Elf *e, *p;
	TAILQ_FOREACH(e, map->list, nodes) {
		// --- Search adjacent ---
		found = false;
		for (y = -1; y <= 1; y++) {
			for (x = -1; x <= 1; x++) {
				if (x == 0 && y == 0) continue;
				if (index_has(map->index, e->x + x, e->y + y)) {
					found = true;
					break;
				}
			}
			if (found) break;
		}
		if (!found) {
			e->px = e->x; e->py = e->y;
			index_add(map->prop, e, e->x, e->y);
			continue;
		}

		// --- Look at the four directions ---
		for (n = 0; n < 4; n++) {
			i = (n + map->round) % 4;
			if (i == 0) { // North
				if (index_has(map->index, e->x - 1, e->y - 1) ||
					index_has(map->index, e->x    , e->y - 1) ||
					index_has(map->index, e->x + 1, e->y - 1)) continue;
				e->py--; break;
			} else if (i == 1) { // South
				if (index_has(map->index, e->x - 1, e->y + 1) ||
					index_has(map->index, e->x    , e->y + 1) ||
					index_has(map->index, e->x + 1, e->y + 1)) continue;
				e->py++; break;
			} else if (i == 2) { // West
				if (index_has(map->index, e->x - 1, e->y - 1) ||
					index_has(map->index, e->x - 1, e->y    ) ||
					index_has(map->index, e->x - 1, e->y + 1)) continue;
				e->px--; break;
			} else if (i == 3) { // East
				if (index_has(map->index, e->x + 1, e->y - 1) ||
					index_has(map->index, e->x + 1, e->y    ) ||
					index_has(map->index, e->x + 1, e->y + 1)) continue;
				e->px++; break;
			} else {
				fprintf(stderr, "Unreachable!\n");
				exit(EXIT_FAILURE);
			}
		}

		if ((e->x != e->px || e->y != e->py) && !index_add(map->prop, e, e->px, e->py)) {
			p = index_get(map->prop, e->px, e->py);
			e->px = e->x; e->py = e->y;
			p->px = p->x; p->py = p->y;
		}
	}

	// --- Move ---
	index_clear(map->index);
	TAILQ_FOREACH(e, map->list, nodes) {
		if (e->x != e->px || e->y != e->py) {
			e->x = e->px; e->y = e->py;
			result = true;
		}
		index_add(map->index, e, e->x, e->y);
	}

	// --- Free resources --
	index_clear(map->prop);

	map->round++;
	return result;
}

// Counts the empty tiles
int map_empty_count(Map* map) {
	int total =
		(map->index->max_x - map->index->min_x + 1) *
		(map->index->max_y - map->index->min_y + 1);
	return total - map->count;
}

// Prints a map into the console
void map_print(Map* map) {
	int x, y;
	int min_x = map->index->min_x;
	int max_x = map->index->max_x;
	int min_y = map->index->min_y;
	int max_y = map->index->max_y;
	printf("x: [%d .. %d], y: [%d .. %d]\n", min_x, max_x, min_y, max_y);
	printf("count: %d\n", map->count);
	char c;
	for (y = min_y; y <= max_y; y++) {
		for (x = min_x; x <= max_x; x++) {
			c = (index_get(map->index, x, y) == NULL) ? '.' : '#';
			printf("%c", c);
		}
		printf("\n");
	}
}

// Free maps resources
void map_free(Map* map) {
	Elf* elf;
	index_free(map->index);
	index_free(map->prop);
	while (!TAILQ_EMPTY(map->list)) {
		elf = TAILQ_FIRST(map->list);
		TAILQ_REMOVE(map->list, elf, nodes);
		free(elf); elf = NULL;
	}
	free(map->list);
	free(map);
}
