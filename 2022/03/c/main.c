#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"

// --- Finds the dupliacte char ---
char find_duplicate(char* p1, char* p2, char* p3) {
	while(1) {
		if (*p1 == 0) {
			fprintf(stderr, "Duplicated not found!");
			exit(EXIT_FAILURE);
		}
		char* p = p2;
		while(1) {
			if (*p == 0) break;
			if (*p1 == *p++) {
				if (p3 == NULL) return *p1;
				p = p3;
				while(1) {
					if (*p == 0) break;
					if (*p1 == *p++) return *p1;
				}
				break;
			}
		}
		p1++;
	}
}

// --- Calculates the priority ---
int get_priority(char c) {
	if (c >= 'a' && c <= 'z') return c - 'a' + 1;
	if (c >= 'A' && c <= 'Z') return c - 'A' + 27;
	fprintf(stderr, "Usupported char: %c\n", c);
	exit(EXIT_FAILURE);
}

// --- Main function ---
int main(void) {
	int totals[] = { 0, 0 };

	READ_BY_LINE_HEAD(file_t)
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE);
	char line[file.max];
	char buffer1[file.max];
	char buffer2[file.max];
	int count = 0;

	READ_BY_LINE_WHILE(file, line)
		// --- Puzzle 1 ---
		size_t len = strlen(line);
		char* p1 = &line[0];
		char* p2 = &line[len / 2];
		char c = find_duplicate(p1, p2, NULL);
		totals[0] += get_priority(c);

		// --- Puzzle 2 ---
		switch(count++ % 3) {
			case 0: strcpy(buffer1, line); break;
			case 1: strcpy(buffer2, line); break;
			case 2:
				c = find_duplicate(&buffer1[0], &buffer2[0], &line[0]);
				totals[1] += get_priority(c);
				break;
		}
	READ_BY_LINE_DONE(file);

	printf("Part 1. Total sum of the priorities: %d\n", totals[0]);
	printf("Part 1. Total sum of the item types: %d\n", totals[1]);
}
