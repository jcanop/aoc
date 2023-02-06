#include <stdio.h>
#include <stdlib.h>
#include "./monkey.h"

struct Monkey* monkey_create(void) {
	struct Monkey* m = malloc(sizeof(struct Monkey));
	m->id = -1;
	int i; for (i = 0; i < ITEMS_MAX; i++) m->items[i] = 0;
	m->items_idx = 0;
	m->operator ='?';
	m->operand = -1;
	m->divisible = 0;
	m->throw_true = -1;
	m->throw_false = -1;
	m->inspects = 0;
	return m;
}

void monkey_add_item(struct Monkey* m, long item) {
	if (m->items_idx == ITEMS_MAX) {
		fprintf(stderr, "Items overflow!");
		exit(EXIT_FAILURE);
	}
	m->items[m->items_idx++] = item;
}

long monkey_pop_item(struct Monkey* m) {
	if (m->items_idx == 0) {
		fprintf(stderr, "Items underflow!");
		exit(EXIT_FAILURE);
	}
	m->items_idx--;
	return m->items[m->items_idx];
}

void monkey_print(const struct Monkey* m) {;
	printf("Monkey %d:\n", m->id);
	printf("  Items:");
	for (int i = 0; i < m->items_idx; i++) printf(" %ld", m->items[i]); printf("\n");
	printf("  Operation: new = old %c %ld\n", m->operator, m->operand);
	printf("  Test: divisible by %ld\n", m->divisible);
	printf("    If true: throw to monkey %d\n", m->throw_true);
	printf("    If false: throw to monkey %d\n", m->throw_false);
}
