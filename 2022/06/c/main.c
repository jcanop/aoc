#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define C_PACKET 4
#define C_MESSAGE 14

// Indicates if a an group oc chars are unique.
bool unique(char* s, int len) {
	int i, j;
	for (i = 0; i < len; i++) {
		for (j = i + 1; j < len; j++) {
			if (*(s + j) == 0) {
				fprintf(stderr, "String overflow!");
				exit(EXIT_FAILURE);
			}
			if (*(s + i) == *(s + j)) return false;
		}
	}
	return true;
}

// Search the stream of data for a marker.
int find(char* s, int c) {
	int len = strlen(s);
	if (len < c) return -1;
	int i; for (i = 0; i < len - c; i++) {
		if (*(s + i) == 0) {
			fprintf(stderr, "String overflow!");
			exit(EXIT_FAILURE);
		}
		if (unique((s + i), c)) return i + c;
	}
	return -1;
}

// --- Main function ---
int main(void) {
	READ_FULL(text, INPUT_FILE);
	int p = find(text, C_PACKET);
	printf("Part 1. Start-of-packet marker: %d\n", p);
	int m = find(text, C_MESSAGE);
	printf("Part 2. Start-of-message marker: %d\n", m);

	return EXIT_SUCCESS;
}
