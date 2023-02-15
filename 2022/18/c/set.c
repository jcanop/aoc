#include <stdlib.h>
#include <string.h>
#include <set.h>

void hash(unsigned char id[], uint x, uint y, uint z) {
	memcpy(id, &x, INT_SIZE);
	memcpy(id + INT_SIZE, &y, INT_SIZE);
	memcpy(id + 2 * INT_SIZE, &z, INT_SIZE);
}

SetNode* set_create_node() {
	SetNode* ptr = malloc(sizeof(SetNode));
	int i; for (i = 0; i < 256; i++) ptr->next[i] = NULL;
	return ptr;
}

Set* set_create() {
	Set* set = malloc(sizeof(Set));
	int i; for (i = 0; i < INT_SIZE * 3; i++) set->id[i] = 0;
	set->root = set_create_node();
	return set;
}

void set_add(Set* set, uint x, uint y, uint z) {
	hash(set->id, x, y, z);
	SetNode* ptr = set->root;
	int i; for (i = 0; i < INT_SIZE * 3; i++) {
		if (ptr->next[set->id[i]] == NULL) ptr->next[set->id[i]] = set_create_node();
		ptr = ptr->next[set->id[i]];
	}
}

bool set_contains(Set* set,  uint x, uint y, uint z) {
	hash(set->id, x, y, z);
	SetNode* ptr = set->root;
	int i; for (i = 0; i < INT_SIZE * 3; i++) {
		if (ptr->next[set->id[i]] == NULL) return false;
		ptr = ptr->next[set->id[i]];
	}
	return true;
}

void set_add_all_rec(SetNode* dst, SetNode* src) {
	int i; for (i = 0; i < 256; i++) {
		if (src->next[i] != NULL) {
			if (dst->next[i] == NULL) dst->next[i] = set_create_node();
			set_add_all_rec(dst->next[i], src->next[i]);
		}
	}
}

void set_add_all(Set* dst, Set* src) {
	set_add_all_rec(dst->root, src->root);
}

void set_do_rec(Set* set, SetNode* node, void (*f)(uint x, uint y, uint z), int level) {
	if (level == INT_SIZE * 3 - 1) {
		uint x, y, z;
		memcpy(&x, &set->id[0], INT_SIZE);
		memcpy(&y, &set->id[INT_SIZE], INT_SIZE);
		memcpy(&z, &set->id[2 * INT_SIZE], INT_SIZE);
		f(x, y, z);
		return;
	}
	int i; for (i = 0; i < 256; i++) {
		if (node->next[i] != NULL) {
			memcpy(&set->id[level], &i, 1);
			set_do_rec(set, node->next[i], f, level + 1);
		}
	}
}

void set_do(Set* set, void (*f)(uint x, uint y, uint z)) {
	set_do_rec(set, set->root, f, 0);
}

void set_free_node(SetNode* node) {
	int i; for (i = 0; i < 256; i++) {
		if (node->next[i] != NULL) set_free_node(node->next[i]);
	}
	free(node);
}

void set_free(Set* set) {
	set_free_node(set->root);
	free(set);
}
