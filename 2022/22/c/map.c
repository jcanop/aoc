#include <stdbool.h>
#include <string.h>
#include <file_reader.h>
#include <map.h>

// Prints the map in the console
void map_print(Map* m) {
	int x, y;
	printf("postion: %d, %d\ndirection: %d\n", m->x, m->y, m->d);
	printf("grid:\n");
	for (y = 0; y < M_HEIGHT; y++) {
		for (x = 0; x < M_WIDTH; x++) printf("%c", m->grid[y][x]);
		printf("\n");
	}
	printf("commands: %s", m->cmds);
}

// Loads the map from the input file into the map
int map_load(Map* m, char* filename) {
	m->x = 0; m->y = 0; m->d = EAST;

	int x, y, len;
	bool search = true;
	bool done = false;
	for (x = 0; x < M_HEIGHT; x++) for (y = 0; y < M_WIDTH; y++) m->grid[x][y] = EMPTY;

	READ_BY_LINE_HEAD(file_t)
	READ_BY_LINE_INIT(file, file_t, filename)
	char line[file.max];
	y = 0;
	READ_BY_LINE_WHILE(file, line)
		len = strlen(line);
		if (!done) {
			for (x = 0; x < len; x++) {
				m->grid[y][x] = line[x];
				if (search && line[x] == OPEN) {
					m->x = x; m->y = y;
					search = false;
				}
			}
			if (++y == M_HEIGHT) done = true;
		} else if (len > 0) {
			m->cmds = malloc(len+ 1);
			strcpy(m->cmds, line);
		}
	READ_BY_LINE_DONE(file);

	return EXIT_SUCCESS;
}

// Tokenizer next element
bool tokenizer_next(Tokenizer* t, char* buffer) {
	t->idx = 0;
	while (*t->ptr != '\0') {
		if (*t->ptr == 'L' || *t->ptr == 'R') {
			if (t->idx == 0) {
				buffer[0] = *t->ptr;
				buffer[1] = '\0';
				t->ptr++;
				return true;
			}
			buffer[t->idx] ='\0';
			return true;
		}
		buffer[t->idx++] = *t->ptr;
		t->ptr++;
	}
	if (t->idx > 0) {
		buffer[t->idx] ='\0';
		return true;
	}
	return false;
}

// Follows the path
void map_path(Map* m, void (*f)(Map*, int)) {
	Tokenizer tokenizer;
	tokenizer.ptr = m->cmds;
	char buffer[10];
	while (tokenizer_next(&tokenizer, buffer)) {
		if (strcmp(buffer, "L") == 0) {
			m->d = (m->d == 0) ? 3 : m->d - 1;
		} else if (strcmp(buffer, "R") == 0) {
			m->d = (m->d == 3) ? 0 : m->d + 1;
		} else {
			(*f)(m, atoi(buffer));
		}
	}
}

// Calcualtes the password
long map_password(Map* m) {
	return 1000L * (m->y + 1L) + 4L * (m->x + 1L) + m->d;
}

// Free the map and its resources
void map_free(Map* m) {
	free(m->cmds);
	m->cmds = NULL;
	free(m);
}
