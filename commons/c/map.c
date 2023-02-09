#include "limits.h"
#include "map.h"

struct MapNodeRoot* map_create(void) {
	struct MapNodeRoot* m = malloc(sizeof(struct MapNodeRoot));
	int i;
	for (i = 0; i < INT_SIZE; i++) m->buffer[i] = 0;
	for (i = 0; i < 256; i++) m->childs[i] = NULL;
	m->min_x = INT_MAX; m->max_x = INT_MIN;
	m->min_y = INT_MAX; m->max_y = INT_MIN;
	return m;
}

struct MapNode* map_create_node(void) {
	struct MapNode* m = malloc(sizeof(struct MapNode));
	m->c = 0;
	int i; for (i = 0; i < 256; i++) m->childs[i] = NULL;
	return m;
}

void map_put(struct MapNodeRoot* root, int x, int y, char c) {
	MAP_HASH_XY(x, y);
	unsigned char idx = root->buffer[0] & 0xFF;
	if (root->childs[idx] == NULL) {
		root->childs[idx] = map_create_node();
	}
	struct MapNode* ptr = root->childs[idx];

	int i; for (i = 1; i < INT_SIZE * 2; i++) {
		idx = root->buffer[i] & 0xFF;
		if (ptr->childs[idx] == NULL) {
			ptr->childs[idx] = map_create_node();
		}
		ptr = ptr->childs[idx];
	}
	ptr->c = c;
	if (x < root->min_x) root->min_x = x;
	if (x > root->max_x) root->max_x = x;
	if (y < root->min_y) root->min_y = y;
	if (y > root->max_y) root->max_y = y;
}

char* map_get(struct MapNodeRoot* root, int x, int y) {
	MAP_HASH_XY(x, y);
	unsigned char idx = root->buffer[0] & 0xFF;
	if (root->childs[idx] == NULL) return NULL;
	struct MapNode* ptr = root->childs[idx];

	int i; for (i = 1; i < INT_SIZE * 2; i++) {
		idx = root->buffer[i] & 0xFF;
		if (ptr->childs[idx] == NULL) return NULL;
		ptr = ptr->childs[idx];
	}
	return &ptr->c;
}

void map_free_node(struct MapNode* node) {
	int i; for (i = 0; i < 256; i++) {
		if (node->childs[i] != NULL) map_free_node(node->childs[i]);
	}
	free(node);
}

void map_free(struct MapNodeRoot* root) {
	int i; for (i = 0; i < 256; i++) {
		if (root->childs[i] != NULL) map_free_node(root->childs[i]);
	}
	free(root);
}
