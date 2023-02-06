#include <string.h>
#include <arrays.h>
#include <file_reader.h>
#include "./monkey.h"

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define M_SIZE sizeof(struct Monkey)

// --- Structs ---
READ_BY_LINE_HEAD(file_t)

// Play a round for a monkey
void play(int monkey_idx, struct Monkey* group[], int relief, long gcd) {
	struct Monkey* m = group[monkey_idx];
	while (m->items_idx > 0) {
		m->inspects++;
		long item = monkey_pop_item(m);
		long value = m->operand == 0 ? item : m->operand;
		switch (m->operator) {
			case '+': item += value; break;
			case '*': item *= value; break;
			default:
				fprintf(stderr, "Unsupported operator: %c\n", m->operator);
				exit(EXIT_FAILURE);
		}
		switch (relief) {
			case 1: item /= 3L; break;
			case 2: item %= gcd; break;
			default:
				fprintf(stderr, "Unsupported relief: %lu\n", gcd);
				exit(EXIT_FAILURE);
		}
		int i = (item % m->divisible == 0) ? m->throw_true : m->throw_false;
		monkey_add_item(group[i], item);
	}
}


// --- Main function ---
int main(void) {
	// --- Variables ---
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	char line[file.max];
	int count = (file.rows + 1) / 7;
	struct Monkey* group_1[count];
	struct Monkey* group_2[count];
	long gcd = 1;
	int row = 0;
	char str[file.max];
	char* token;

	// --- Creates the monkeys for the group 1 ---
	int i; for (i = 0; i < count; i++) group_1[i] = monkey_create();

	// --- Read and parse the input file ---
	READ_BY_LINE_WHILE(file, line)
		struct Monkey* m = group_1[row / 7];
		switch(row++ % 7) {
			case 0: sscanf(line, "Monkey %d:", &m->id); break;
			case 1:
				token = strtok(&line[18], ",");
				while (token != NULL) {
					monkey_add_item(m, atol(token));
					token = strtok(NULL, ",");
				}
				break;
			case 2:
				sscanf(line, "  Operation: new = old %c %s", &m->operator, str);
				m->operand = (strcmp(str, "old") == 0) ? 0 : atoi(str);
				break;
			case 3:
				sscanf(line, "  Test: divisible by %ld", &m->divisible);
				gcd *= m->divisible;
				break;
			case 4: sscanf(line, "    If true: throw to monkey %d", &m->throw_true); break;
			case 5: sscanf(line, "    If false: throw to monkey %d", &m->throw_false); break;
			case 6: break;
			default: fprintf(stderr, "Unreachable!\n"); exit(EXIT_FAILURE);
		}
	READ_BY_LINE_DONE(file)

	// --- Copy monkeys from group_1 to group_2 ---
	for (i = 0; i < count; i++) {
		group_2[i] = malloc(M_SIZE);
		memcpy(group_2[i], group_1[i], M_SIZE);
	}

	// --- Puzzle 1 ---
	int m; long inspects[count];
	for (i = 0; i < 20; i++) for (m = 0; m < count; m++) play(m, group_1, 1, gcd);
	for (i = 0; i < count; i++) inspects[i] = group_1[i]->inspects;
	array_sort(long, inspects);
	array_reverse(long, inspects);
	long total = inspects[0] * inspects[1];
	printf("1. Level of monkey business: %lu\n", total);

	// --- Puzzle 2 ---
	for (i = 0; i < 10000; i++) for (m = 0; m < count; m++) play(m, group_2, 2, gcd);
	for (i = 0; i < count; i++) inspects[i] = group_2[i]->inspects;
	array_sort(long, inspects);
	array_reverse(long, inspects);
	total = inspects[0] * inspects[1];
	printf("2. Level of monkey business: %lu\n", total);

	// --- Free resources ---
	for (i = 0; i < count; i++) {
		free(group_1[i]); group_1[i] = NULL;
		free(group_2[i]); group_2[i] = NULL;
	}

	return EXIT_SUCCESS;
}
