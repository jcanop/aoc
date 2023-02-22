#include <stdio.h>
#include <stdlib.h>
#include <lib.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define ROOT "root"
#define ME "humn"

// --- Main function ---
int main(void) {
	Resolver* r;

	// --- Puzzle 1 ---
	r = malloc(sizeof(Resolver));
	resolver_init(r);
	resolver_load(r, INPUT_FILE);
	long result = resolver_solve_for(r, ROOT);
	printf("1. Number that will the monkey root yell: %ld\n", result);
	resolver_free(r); r = NULL;

	// --- Puzzle 2 ---
	r = malloc(sizeof(Resolver));
	resolver_init(r);
	resolver_load(r, INPUT_FILE);
	resolver_update_for_puzzle2(r, ROOT, ME);
	result = resolver_solve_for(r, ME);
	printf("2. Number that I yell to pass root's equality test: %ld\n", result);
	resolver_free(r); r = NULL;

	return EXIT_SUCCESS;
}
