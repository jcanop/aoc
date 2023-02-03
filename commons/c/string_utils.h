#ifndef STRING_UTIL_H_
#define STRING_UTIL_H_

#include <stdbool.h>
#include <string.h>
bool starts_with(const char* pre, const char* str) {
	size_t len_pre = strlen(pre);
	size_t len_str = strlen(str);
	return len_str < len_pre ? false : memcmp(pre, str, len_pre) == 0;
}

#endif /* STRING_UTIL_H_ */
