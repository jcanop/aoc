#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// --- Constants ---
const char INPUT_FILE[] = "../input/input.txt";
const int LINE_MAX_LEN = 100;

// --- Main function ---
int main(void) {
	int max[] = { 0, 0, 0 };

	// --- Read and parse the input file ---
	char line[LINE_MAX_LEN];
	FILE* file = fopen(INPUT_FILE, "r");
	if (file == NULL) {
		printf("Not able to open the input file.");
		return 1;
	}

	int count = 0;
	while(fgets(line, LINE_MAX_LEN, file)) {
		int i = atoi(line);
		if (i == 0) {
			if (count > max[0]) {
				max[2] = max[1];
				max[1] = max[2];
				max[0] = count;
			} else if (count > max[1]) {
				max[2] = max[1];
				max[1] = count;
			} else if (count > max[2]) {
				max[2] = count;
			}
			count = 0;
		} else {
			count += i;
		}
	}
	fclose(file);

	// --- Puzzle 1 ---
	printf("1. The Elf carrying the most Calories, is carrying %d Calories.\n", max[0]);

	// --- Puzzle 2 ---
	int total = max[0] + max[1] + max[2];
	printf("2. The top 3 Elves carrying the most Calories, are carrying %d Calories.\n", total);
}
