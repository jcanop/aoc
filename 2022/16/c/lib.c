#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <set.h>
#include <lib.h>

#define PUZZLE_1_TIME 30
#define PUZZLE_2_TIME 26

void layout_free(Layout* layout) {
	if (layout->flows != NULL) set_free(layout->flows);
	if (layout->distances != NULL) set_free(layout->distances);
}

SetNode* floyd_warshal(char valves[][ID_SIZE], int count, Tunnels* tunnels) {
	SetNode* distances = set_create();
	int i, j, k;
	for (i = 0; i < count; i++) {
		char* from = valves[i];
		for (j = 0; j < count; j++) {
			char* to = valves[j];
			set_put_distance(distances, from, to, count + 1);
		}
	}

	struct Tunnels *t, *tmpt;
	HASH_ITER(hh, tunnels, t, tmpt) {
		for (i = 0; i < MAX_TO; i++) {
			if (t->leads[i][0] == 0) break;
			set_put_distance(distances, t->id, t->leads[i], 1);
		}
	}

	for (i = 0; i < count; i++) {
		set_put_distance(distances, valves[i], valves[i], 0);
	}

	struct Distance *d1, *d2;
	for (i = 0; i < count; i++) {
		char* via = valves[i];
		for (j = 0; j < count; j++) {
			char* from = valves[j];
			for (k = 0; k < count; k++) {
				char* to = valves[k];

				int dist = *set_get_distance(distances, from, via) +
					*set_get_distance(distances, via, to);
				if (dist < *set_get_distance(distances, from, to)) {
					set_put_distance(distances, from, to, dist);
				}
			}
		}
	}

	return distances;
}

int do_dfs1(Layout* layout, char* current, int time, int total, SetNode* open) {
	int max = total + set_total_flow(open) * (PUZZLE_1_TIME - time);
	int i = 0; for (i = 0; i < layout->useful_len; i++) {
		char* next = layout->useful[i];
		if (set_contains(open, next)) continue;
		int delta = *set_get_distance(layout->distances, current, next) + 1;
		if (time + delta >= PUZZLE_1_TIME) continue;
		int new_total = total + delta * set_total_flow(open);
		set_put_flow(open, next, *set_get_flow(layout->flows, next));
		int value = do_dfs1(layout, next, time + delta, new_total, open);
		if (max < value) max = value;
		set_remove(open, next);
	}
	return max;
}

int dfs1 (Layout* layout) {
	SetNode*  open = set_create();
	int total = do_dfs1(layout, "AA", 0, 0, open);
	set_free(open);
	return total;
}

int do_dfs2(Layout* layout, char* current, bool elephant, int time, int total, SetNode* open, SetNode* useful, int total_flow) {
	int max = total + total_flow * (PUZZLE_2_TIME - time);
	if (!elephant) {
		SetNode* new_open = set_create();
		SetNode* new_candidates = set_create();
		set_difference(new_candidates, useful, open);
		int max_elephant = do_dfs2(layout, "AA", true, 0, 0, new_open, new_candidates, 0);
		max = total + total_flow * (PUZZLE_2_TIME - time) + max_elephant;
		set_free(new_candidates);
		set_free(new_open);
	}
	int i = 0; for (i = 0; i < layout->useful_len; i++) {
		char* next = layout->useful[i];
		if (!set_contains(useful, next)) continue;
		if (set_contains(open, next)) continue;
		int delta = *set_get_distance(layout->distances, current, next) + 1;
		if (time + delta >= PUZZLE_2_TIME) continue;
		int new_total = total + delta * total_flow;
		set_put(open, next);
		int next_flow = *set_get_flow(layout->flows, next);
		total_flow += next_flow;
		int value = do_dfs2(layout, next, elephant, time + delta, new_total, open, useful, total_flow);
		if (max < value) max = value;
		set_remove(open, next);
		total_flow -= next_flow;
	}
	return max;
}

int dfs2(Layout* layout) {
	SetNode* open = set_create();
	SetNode* useful = set_create();
	int i = 0; for (i = 0; i < layout->useful_len; i++) {
		char* id = layout->useful[i];
		set_put(useful, id);
	}
	int total = do_dfs2(layout, "AA", false, 0, 0, open, useful, 0);
	set_free(open);
	set_free(useful);
	return total;
}
