#include <stdio.h>
#include <stdlib.h>
#include "./lib.h"

// --- Enums parsers ---
enum Shape parse_shape(char c) {
	switch (c) {
		case 'A':
		case 'X': return ROCK;
		case 'B':
		case 'Y': return PAPER;
		case 'C':
		case 'Z': return SCISSORS;
		default:
			fprintf(stderr, "Unsupported shape: %c", c);
			exit(EXIT_FAILURE);
	}
}

enum Result parse_result(char c) {
	switch(c) {
		case 'X': return LOSE;
		case 'Y': return DRAW;
		case 'Z': return WIN;
		default:
			fprintf(stderr, "Unsupported result: %c", c);
			exit(EXIT_FAILURE);
	}
}

// --- Play methods ---
enum Result play(enum Shape me, enum Shape other) {
	if (me == ROCK) {
		switch(other) {
			case ROCK: return DRAW;
			case PAPER: return LOSE;
			case SCISSORS: return WIN;
			default:
				fprintf(stderr, "Unsupported shape: %d", other);
				exit(EXIT_FAILURE);
		}
	} else if (me == PAPER) {
		switch(other) {
			case ROCK: return WIN;
			case PAPER: return DRAW;
			case SCISSORS: return LOSE;
			default:
				fprintf(stderr, "Unsupported shape: %d", other);
				exit(EXIT_FAILURE);
		}
	} else if (me == SCISSORS) {
		switch(other) {
			case ROCK: return LOSE;
			case PAPER: return WIN;
			case SCISSORS: return DRAW;
			default:
				fprintf(stderr, "Unsupported shape: %d", other);
				exit(EXIT_FAILURE);
		}
	}
	fprintf(stderr, "Unsupported shape: %d", me);
	exit(EXIT_FAILURE);
}

enum Shape shape_to(enum Shape other, enum Result result) {
	if (other == ROCK) {
		switch(result) {
			case WIN:  return PAPER;
			case DRAW: return ROCK;
			case LOSE: return SCISSORS;
			default:
				fprintf(stderr, "Unsupported result: %d", result);
				exit(EXIT_FAILURE);
		}
	} else if (other == PAPER) {
		switch(result) {
			case WIN:  return SCISSORS;
			case DRAW: return PAPER;
			case LOSE: return ROCK;
			default:
				fprintf(stderr, "Unsupported result: %d", result);
				exit(EXIT_FAILURE);
		}
	} else if (other == SCISSORS) {
		switch(result) {
			case WIN:  return ROCK;
			case DRAW: return SCISSORS;
			case LOSE: return PAPER;
			default:
				fprintf(stderr, "Unsupported result: %d", result);
				exit(EXIT_FAILURE);
		}
	}
	fprintf(stderr, "Unsupported shape: %d", other);
	exit(EXIT_FAILURE);
}
