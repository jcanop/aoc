#include <stdio.h>
#include "./set.h"

// --- Init ---
SetNode* set_create_node() {
	SetNode* node = malloc(sizeof(SetNode));
	node->value = 0;
	node->is_leaf = false;
	int i; for (i = 0; i < SET_MAX; i++) node->childs[i] = NULL;
	return node;
}

SetNode* set_create() {
	return set_create_node();
}

// --- Flow ---
void set_put_flow(SetNode* root, const char* id, int flow) {
	SetNode* ptr = root;
	int i; for (i = 0; i < ID_LEN; i++) {
		int n = id[i] - OFFSET;
		if (ptr->childs[n] == NULL) ptr->childs[n] = set_create_node();
		ptr = ptr->childs[n];
	}
	ptr->value = flow;
	ptr->is_leaf = true;
}

int* set_get_flow(SetNode* root, const char* id) {
	SetNode* ptr = root;
	int i; for (i = 0; i < ID_LEN; i++) {
		int n = id[i] - OFFSET;
		if (ptr->childs[n] == NULL) return NULL;
		ptr = ptr->childs[n];
	}
	return &ptr->value;
}

int set_total_flow(SetNode* root) {
	int total = 0;
	int i; for (i = 0; i < SET_MAX; i++) {
		if (root->childs[i] != NULL) total += set_total_flow(root->childs[i]);
	}
	if (!root->is_leaf) return total;
	return total + root->value;
}

// --- Distance  ---
void set_put_distance(SetNode* root, const char* from, const char* to, int distance) {
	SetNode* ptr = root;
	int i; for (i = 0; i < ID_LEN * 2; i++) {
		int n = (i < ID_LEN ? from[i] : to[i - ID_LEN]) - OFFSET;
		if (ptr->childs[n] == NULL) ptr->childs[n] = set_create_node();
		ptr = ptr->childs[n];
	}
	ptr->value = distance;
	ptr->is_leaf = true;
}

int* set_get_distance(SetNode* root, const char* from, const char* to) {
	SetNode* ptr = root;
	int i; for (i = 0; i < ID_LEN * 2; i++) {
		int n = (i < ID_LEN ? from[i] : to[i - ID_LEN]) - OFFSET;
		if (ptr->childs[n] == NULL) return NULL;
		ptr = ptr->childs[n];
	}
	return &ptr->value;
}

// --- General ---
void set_put(SetNode* root, const char* id) {
	set_put_flow(root, id, 0);
}

bool set_contains(SetNode* root, const char* id) {
	SetNode* ptr = root;
	int i; for (i = 0; i < ID_LEN; i++) {
		int n = id[i] - OFFSET;
		if (ptr->childs[n] == NULL) return false;
		ptr = ptr->childs[n];
	}
	return true;
}

void set_remove(SetNode* root, const char* id) {
	SetNode* ptr = root;
	int i; for (i = 0; i < ID_LEN; i++) {
		int n = id[i] - OFFSET;
		if (i == 1) {
			free(ptr->childs[n]);
			ptr->childs[n] = NULL;
			return;
		}
		if (ptr->childs[n] == NULL) return;
		ptr = ptr->childs[n];
	}
}

void set_clone(SetNode* dst, const SetNode* src) {
	dst->value = src->value;
	dst->is_leaf = src->is_leaf;
	int i = 0; for (i = 0; i < SET_MAX; i++) {
		if (src->childs[i] != NULL) {
			dst->childs[i] = set_create_node();
			set_clone(dst->childs[i], src->childs[i]);
		}
	}
}

void set_difference(SetNode* dst, const SetNode* s1, const SetNode* s2) {
	if (s1->is_leaf) {
		dst->value = s1->value;
		dst->is_leaf = true;
		return;
	}
	int i = 0; for (i = 0; i < SET_MAX; i++) {
		if (s1->childs[i] != NULL && (!s1->childs[i]->is_leaf ||
			(s1->childs[i]->is_leaf && (s2 == NULL || s2->childs[i] == NULL)))) {

			dst->childs[i] = set_create_node();
			SetNode* ptr = NULL;
			if (s2 != NULL && s2->childs[i] != NULL) ptr = s2->childs[i];
			set_difference(dst->childs[i], s1->childs[i], ptr);;
		}
	}
}

void set_difference2(SetNode* dst, const SetNode* s1, const SetNode* s2) {
	dst->value = s1->value;
	dst->is_leaf = s1->is_leaf;
	int i = 0; for (i = 0; i < SET_MAX; i++) {
		if (s1->childs[i] != NULL &&
			(!s1->is_leaf || (s1->is_leaf && (s2 == NULL || s2->childs[i] == NULL)))) {

			dst->childs[i] = set_create_node();
			SetNode* ptr = NULL;
			if (s2 != NULL && s2->childs[i] != NULL) ptr = s2->childs[i];
			set_difference(dst->childs[i], s1->childs[i], ptr);;
		}
	}
}

void set_free(SetNode* root) {
	int i = 0; for (i = 0; i < SET_MAX; i++) {
		if (root->childs[i] != NULL) set_free(root->childs[i]);
	}
	free(root);
}
