#ifndef FILE_READER_H_
#define FILE_READER_H_

//
// Reads a file line by line
//
#define READ_BY_LINE_INIT(line, rows, filename)\
	FILE* __FILE = fopen(filename, "r");\
	if (__FILE == NULL) {\
		fprintf(stderr, "Not able to open file: %s\n", filename);\
		return EXIT_FAILURE;\
	}\
	size_t rows = 0;\
	size_t __MAX = 0;\
	size_t __COUNT = 0;\
	while(1) {\
		int c = fgetc(__FILE);\
		if (c == EOF || c == '\n') {\
			if (__MAX < __COUNT) __MAX = __COUNT;\
			__COUNT = 0;\
			if (c == EOF) break;\
			rows++;\
		} else {\
			__COUNT++;\
		}\
	}\
	__MAX += 2;\
	if (fseek(__FILE, 0, SEEK_SET) != 0) {\
		fprintf(stderr, "Not able to set position: 0\n");\
		return EXIT_FAILURE;\
	}\
	char line[__MAX];

#define READ_BY_LINE_WHILE(line)\
	while(fgets(line, __MAX, __FILE)) {\
		line[strcspn(line, "\n")] = 0;

#define READ_BY_LINE_DONE()\
	}\
	fclose(__FILE);

//
// Reads a whole file into a string
//
#define READ_FULL(text, filename)\
	FILE* __FILE = fopen(filename, "r");\
	if (__FILE == NULL) {\
		fprintf(stderr, "Not able to open file: %s\n", filename);\
		return EXIT_FAILURE;\
	}\
	fseek(__FILE, 0, SEEK_END);\
	long __FSIZE = ftell(__FILE);\
	fseek(__FILE, 0, SEEK_SET);\
	char text[__FSIZE + 1];\
	fread(text, __FSIZE, 1, __FILE);\
	fclose(__FILE);\
	text[__FSIZE] = 0;

#endif /* FILE_READER_H_ */
