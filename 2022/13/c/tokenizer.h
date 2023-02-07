#ifndef TOKENIZER_H_
#define TOKENIZER_H_

#include <stdbool.h>

struct Tokenizer {
	char* str;
	char* ptr;
	size_t len;
};

// Creates a new tokenizer
struct Tokenizer tokenizer_create(const char* str);

// Gets the next token
bool tokenizer_next(struct Tokenizer* t, char* buffer);

// Free the resources allocated by this tokenizer
void tokenizer_free(struct Tokenizer* t);

#endif /* TOKENIZER_H_ */
