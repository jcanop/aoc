#ifndef FILE_READER_H_
#define FILE_READER_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//
// Reads a file line by line
//
// Example:
//
// READ_BY_LINE_HEAD(file_t)
// READ_BY_LINE_INIT(file, file_t, "input.txt")
// char line[file.max];
// READ_BY_LINE_WHILE(file, line)
//     printf("%s\n", line);
// READ_BY_LINE_DONE(file)
//
#define READ_BY_LINE_HEAD(type)\
	struct type { FILE* file; size_t rows; size_t max; };

#define READ_BY_LINE_INIT(name, type, filename)\
	struct type name;\
	name.file = fopen(filename, "r");\
	if (name.file == NULL) {\
		fprintf(stderr, "Not able to open file: %s\n", filename);\
		return EXIT_FAILURE;\
	}\
	name.rows = 0;\
	name.max = 0;\
	{\
		size_t count = 0;\
		while(1) {\
			int c = fgetc(name.file);\
			if (c == EOF || c == '\n') {\
				if (name.max < count) name.max = count;\
				count = 0;\
				if (c == EOF) break;\
				name.rows++;\
			} else {\
				count++;\
			}\
		}\
		name.max += 2;\
		if (fseek(name.file, 0, SEEK_SET) != 0) {\
			fprintf(stderr, "Not able to set position: 0\n");\
			return EXIT_FAILURE;\
		}\
	}\

#define READ_BY_LINE_WHILE(name, line)\
	while(fgets(line, name.max, name.file)) {\
		line[strcspn(line, "\n")] = 0;

#define READ_BY_LINE_DONE(name)\
	}\
	fclose(name.file);\

//
// Reads a full file into a single line.
//
// Example:
//
// READ_BY_LINE_HEAD(file_t)
// READ_BY_LINE_INIT(file, file_t, "input.txt")
// char* data;
// READ_BY_LINE_FULL(name, data)
// free(data); data = NULL;
//
#define READ_BY_LINE_FULL(name, data) {\
	char line[name.max];\
	data = malloc((name.max - 2) * name.rows + 1);\
	char* ptr = data;\
	int len = 0;\
	while(fgets(line, name.max, name.file)) {\
		len = strlen(line) - 1;\
		memcpy(ptr, line, len);\
		ptr += len;\
	}\
	*ptr = 0; ptr = NULL;\
	fclose(name.file);\
}

//
// Reads a whole file into a string
//
// Example:
//
// char* text;
// READ_FULL(text, "input.txt")
// free(text);
// text = NULL;
//
#define READ_FULL(text, filename)\
{\
	if (text != NULL) {\
		fprintf(stderr, "Text pointer must be NULL!");\
		return EXIT_FAILURE;\
	}\
	FILE* file = fopen(filename, "r");\
	if (file == NULL) {\
		fprintf(stderr, "Not able to open file: %s\n", filename);\
		return EXIT_FAILURE;\
	}\
	fseek(file, 0, SEEK_END);\
	long fsize = ftell(file);\
	fseek(file, 0, SEEK_SET);\
	text = malloc(fsize + 1);\
	fread(text, fsize, 1, file);\
	fclose(file);\
	text[fsize] = 0;\
}\

#endif /* FILE_READER_H_ */
