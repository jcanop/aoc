#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <file_reader.h>
#include "./tokenizer.h"

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define DIVIDER_1 "[[2]]"
#define DIVIDER_2 "[[6]]"

// --- Structs ---
READ_BY_LINE_HEAD(file_t)

// Surround an string with square brackets.
// String must be big enough!
void to_array(char* str) {
	int idx = 0;
	int len = strlen(str);
	for (int i = len; i > 0; i--) *(str + i) = *(str + i - 1);
	*str = '[';
	*(str + len + 1) = ']';
	*(str + len + 2) = 0;
}

// Compares to lines.
int compare(const char* s1, const char* s2) {
	int i, r;
	struct Tokenizer z1 = tokenizer_create(s1);
	struct Tokenizer z2 = tokenizer_create(s2);
	char t1[z1.len + 2]; for (i = 0; i < z1.len + 2; i++) t1[i] = 0;
	char t2[z2.len + 2]; for (i = 0; i < z2.len + 2; i++) t2[i] = 0;

	while (true) {
		bool b1 = tokenizer_next(&z1, &t1[0]);
		bool b2 = tokenizer_next(&z2, &t2[0]);
		if (!b1 && !b2) { r =  0; goto done; }
		if (!b1)        { r = -1; goto done; }
		if (!b2)        { r =  1; goto done; }
		if (*t1 != '[' && *t2 != '[') {
			r = atoi(t1) - atoi(t2);
			if (r != 0) goto done;
		} else {
			if (*t1 != '[') to_array(t1);
			if (*t2 != '[') to_array(t2);
			r = compare(t1, t2);
			if (r != 0) goto done;
		}
	}
done:
	tokenizer_free(&z1);
	tokenizer_free(&z2);
	return r;
}

// --- Main function ---
int main(void) {
	// --- Variables ---
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	char line[file.max];
	char buffer[file.max];
	char* list[file.rows + 2];
	int list_idx = 0;
	int total = 0;
	int row = 0;

	// --- Read and parse the input file ---
	READ_BY_LINE_WHILE(file, line)
		int id = row / 3 + 1;
		switch (row++ % 3) {
			case 0: strcpy(buffer, line); break;
			case 1: if (compare(&buffer[0], &line[0]) < 0) total += id; break;
		}
		if (strlen(line) > 0) {
			list[list_idx++] = strdup(line);
		}
	READ_BY_LINE_DONE(file)

	// --- Puzle 1 ---
	printf("1. The sum of the indices of the right order pairs: %d\n", total);

	// --- Puzzle 2 --
	list[list_idx++] = strdup(DIVIDER_1);
	list[list_idx++] = strdup(DIVIDER_2);
	int i, j; char* temp;
	for (i = 0; i < list_idx; i++) {
		for (j = i + 1; j < list_idx; j++) {
			if (compare(list[i], list[j]) > 0) {
				temp = list[i];
				list[i] = list[j];
				list[j] = temp;
			}
		}
	}
	total = 1;
	for (i = 0; i < list_idx; i++) {
		if (strcmp(list[i], DIVIDER_1) == 0 ||
			strcmp(list[i], DIVIDER_2) == 0) total *= (i + 1);
	}
	printf("2. The decoder key: %d\n", total);

	// --- Free resources ---
	for (i = 0; i < list_idx; i++) if (list[i] != NULL) { free(list[i]); list[i] = NULL; }

	return EXIT_SUCCESS;
}
