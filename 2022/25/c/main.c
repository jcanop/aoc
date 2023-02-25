#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define BUFFER_SIZE 100

// --- Functions ---
long powi(int base, int power) {
	int i;
	long result = 1;
	for (i = 0; i < power; i++) result *= base;
	return result;
}

void to_snafu(char* buffer, long value) {
	buffer[0] = '\0';
	int i = 1;
	bool overflow;
	while (value > 0) {
		overflow = false;
		switch (value % 5) {
			case 0: buffer[i++] = '0'; break;
			case 1: buffer[i++] = '1'; break;
			case 2: buffer[i++] = '2'; break;
			case 3: buffer[i++] = '='; overflow = true; break;
			case 4: buffer[i++] = '-'; overflow = true; break;
		}
		value /= 5;
		if (overflow) value++;
	}
	int len = i; char tmp;
	for (i = 0; i < len / 2; i++) {
		tmp = buffer[i];
		buffer[i] = buffer[len - i - 1];
		buffer[len - i - 1] = tmp;
	}
}

long from_snafu(char* buffer) {
	long p, result = 0;
	int i, len = strlen(buffer);
	for (i = 0; i < len; i++) {
		p = powi(5, len - i - 1);
		switch (buffer[i]) {
			case '0': break;
			case '1': result += 1 * p; break;
			case '2': result += 2 * p; break;
			case '=': result -= 2 * p; break;
			case '-': result -= 1 * p; break;
		}
	}
	return result;
}


// --- Main function ---
int main(void) {
	// --- Variables ---
	READ_BY_LINE_HEAD(file_t);
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE);
	long total = 0;
	char line[file.max];

	// --- Read and parse the input file ---
	READ_BY_LINE_WHILE(file, line)
		total += from_snafu(line);
	READ_BY_LINE_DONE(file);

	char buffer[BUFFER_SIZE];
	to_snafu(buffer, total);
	printf("SNAFU number to supply to Bob's console: %s\n", buffer);

	return EXIT_SUCCESS;
}
