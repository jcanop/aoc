#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "./tokenizer.h"

struct Tokenizer tokenizer_create(const char* str) {
	struct Tokenizer t = { strdup(str) };
	t.ptr = t.str;
	if (*t.ptr == '[') t.ptr++;
	t.len = strlen(t.str);
	if (*(t.ptr + t.len - 2) == ']') *(t.ptr + t.len - 2) = 0;
	return t;
}

bool tokenizer_next(struct Tokenizer* t, char* buffer) {
	if (buffer == NULL) {
		fprintf(stderr, "Buffer can't be NULL");
		exit(EXIT_FAILURE);
	}

	*buffer = 0;
	int idx = 0;
	int count = 0;
	while (*t->ptr != 0) {
		char c = *t->ptr++;
		if (c == '[') {
			*(buffer + idx++) = c;
			count++;
		} else if (c == ',') {
			if (count == 0) goto finish;
			*(buffer + idx++) = c;
		} else if (c == ']') {
			*(buffer + idx++) = c;
			if (count == 0) goto finish;
			count--;
		} else {
			*(buffer + idx++) = c;
		}
	}

finish:
	if (*buffer == 0) return false;
	*(buffer + idx) = 0;
	return true;
}

void tokenizer_free(struct Tokenizer* t) {
	free(t->str);
	t->str = NULL; t->ptr = NULL;
}
