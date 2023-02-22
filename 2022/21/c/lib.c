#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <regex.h>
#include <file_reader.h>
#include <lib.h>

// Prints an operation struct into the console
void operation_print(Operation* op) {
	if (op == NULL) printf("NULL\n");
	else printf("%s: %s %c %s\n", op->key, op->op1, op->operator, op->op2);
}

// Prints a solution struct into the console
void solution_print(Solution* s) {
	if (s == NULL) printf("NULL\n");
	else printf("%s: %ld\n", s->id, s->value);
}

// Prints a resolver struct into the console
void resolver_print(Resolver* r) {
	printf("queue:\n");
	Operation* o;
	TAILQ_FOREACH(o, &r->queue, nodes) {
		printf("  "); operation_print(o);
	}
	printf("solutions:\n");
	Solution *s, *tmp;
	HASH_ITER(hh, r->solutions, s, tmp) {
		printf("  "); solution_print(s);
	}
}

// Swap a two keys vavlues
char SWAP_TMP[KEY_SIZE];
void swap(char* str1, char* str2) {
	memcpy(&SWAP_TMP, str1, KEY_SIZE);
	memcpy(str1, str2, KEY_SIZE);
	memcpy(str2, &SWAP_TMP, KEY_SIZE);
}

// Solver for a variable
void operation_solve_for_var(Operation* op, int i) {
	if (i == 1) {
		switch (op->operator) {
			case '+': op->operator = '-'; break;
			case '-': op->operator = '+'; break;
			case '*': op->operator = '/'; break;
			case '/': op->operator = '*'; break;
			default: fprintf(stderr, "Unsupported operator: %c\n", op->operator); exit(EXIT_FAILURE);
		}
		swap(op->key, op->op1);
	} else if (i == 2) {
		bool swp = false;
		switch(op->operator) {
			case '+': op->operator = '-'; swp = true; break;
			case '-': break;
			case '*': op->operator = '/'; swp = true; break;
			case '/': break;
			default: fprintf(stderr, "Unsupported operator: %c\n", op->operator); exit(EXIT_FAILURE);
		}
		swap(op->key, op->op2);
		if (swp) swap(op->op1, op->op2);
	} else {
		fprintf(stderr, "Unsupported i: %d", i); exit(EXIT_FAILURE);
	}
}

// Inits a resolver struct
void resolver_init(Resolver* resolver) {
	TAILQ_INIT(&resolver->queue);
	resolver->solutions = NULL;
}

// Loads the content of input file into the resolver
int resolver_load(Resolver* resolver, char* filename) {
	regex_t re;
	int result = regcomp(&re, "^[a-z]{4}: [0-9]+$", REG_EXTENDED);
	if (result) {
		fprintf(stderr, "Could not compile regex\n");
		exit(EXIT_FAILURE);
	}

	READ_BY_LINE_HEAD(file_t)
	READ_BY_LINE_INIT(file, file_t, filename)
	char line[file.max];
	char key[KEY_SIZE], op1[KEY_SIZE], op2[KEY_SIZE];
	char operator;
	long value;
	READ_BY_LINE_WHILE(file, line)
		if (!regexec(&re, line, 0, NULL, 0)) {
			line[4] = ' ';
			sscanf(line, "%s %ld", key, &value);
			Solution* s = malloc(sizeof(Solution));
			strcpy(s->id, key);
			s->value = value;
			HASH_ADD_STR(resolver->solutions, id, s);
		} else {
			line[4] = ' ';
			sscanf(line, "%s %s %c %s", key, op1, &operator, op2);
			Operation* o = malloc(sizeof(Operation));
			strcpy(o->key, key);
			strcpy(o->op1, op1);
			strcpy(o->op2, op2);
			o->operator = operator;
			TAILQ_INSERT_TAIL(&resolver->queue, o, nodes);
		}
	READ_BY_LINE_DONE(file);

	regfree(&re);

	return EXIT_SUCCESS;
}

// Updates the resolver for the second puzzle
void resolver_update_for_puzzle2(Resolver* resolver, char* root, char* me) {
	Operation* op;
	TAILQ_FOREACH(op, &resolver->queue, nodes) {
		if (strcmp(op->key, root) == 0) op->operator = '=';
		if (strcmp(op->op1, me) == 0) operation_solve_for_var(op, 1);
		else if (strcmp(op->op2, me) == 0) operation_solve_for_var(op, 2);
	}
	Solution* s;
	HASH_FIND_STR(resolver->solutions, me, s);
	HASH_DEL(resolver->solutions, s);
	free(s);
}

// Adds a solution into the resolver
void resolver_add_solution(Resolver* resolver, char* key, long value) {
	Operation* o;
	Solution *v1, *v2, *stmp;
	Solution* s = malloc(sizeof(Solution));
	strcpy(s->id, key);
	s->value = value;
	HASH_REPLACE_STR(resolver->solutions, id, s, stmp);
	TAILQ_FOREACH(o, &resolver->queue, nodes) {
		if (strcmp(o->key, key) != 0) continue;
		HASH_FIND_STR(resolver->solutions, o->op1, v1);
		HASH_FIND_STR(resolver->solutions, o->op2, v2);
		if (v1 != NULL && v2 == NULL) operation_solve_for_var(o, 2);
		else if (v1 == NULL && v2 != NULL) operation_solve_for_var(o, 1);
	}
}

// Finds the solution for a variable
long resolver_solve_for(Resolver* resolver, char* id) {
	Operation* op = NULL;
	Solution *v1, *v2, *s;
	long result = 0;
	while (!TAILQ_EMPTY(&resolver->queue)) {
		op = TAILQ_FIRST(&resolver->queue);
		TAILQ_REMOVE(&resolver->queue, op, nodes);
		HASH_FIND_STR(resolver->solutions, op->op1, v1);
		HASH_FIND_STR(resolver->solutions, op->op2, v2);

		if (op->operator == '=') {
			if (v1 != NULL && v2 == NULL)
				resolver_add_solution(resolver, op->op2, v1->value);
			else if (v1 == NULL && v2 != NULL)
				resolver_add_solution(resolver, op->op1, v2->value);
		}

		if (v1 != NULL && v2 != NULL) {
			switch (op->operator) {
				case '+': result = v1->value + v2->value; break;
				case '-': result = v1->value - v2->value; break;
				case '*': result = v1->value * v2->value; break;
				case '/': result = v1->value / v2->value; break;
				case '=': break;
				default: fprintf(stderr, "Unsupported operation %c\n", op->operator); exit(EXIT_FAILURE);
			}
			if (op->operator != '=') resolver_add_solution(resolver, op->key, result);
			free(op);
		} else {
			TAILQ_INSERT_TAIL(&resolver->queue, op, nodes);
		}
	}
	HASH_FIND_STR(resolver->solutions, id, s);
	if (s == NULL) {
		fprintf(stderr, "Solution is NULL!\n");
		exit(EXIT_FAILURE);
	}
	return s->value;
}

// Free the resolver and all its resources
void resolver_free(Resolver* resolver) {
	Operation* o = NULL;
	while(!TAILQ_EMPTY(&resolver->queue)) {
		o = TAILQ_FIRST(&resolver->queue);
		TAILQ_REMOVE(&resolver->queue, o, nodes);
		free(o);
	}
	Solution *s, *tmp;
	HASH_ITER(hh, resolver->solutions, s, tmp) {
		HASH_DEL(resolver->solutions, s);
		free(s);
	}
	free(resolver);
}
