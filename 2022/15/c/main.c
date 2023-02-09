#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <file_reader.h>

// --- Constants ---
#define INPUT_FILE "../input/input.txt"
#define SENSOR 'S'
#define BEACON 'B'
#define PUZZLE_1_LINE 2000000
#define PUZZLE_2_MIN 0
#define PUZZLE_2_MAX 4000000

// --- Macros ---
#define IN_LIMIT(x, y)\
	x >= PUZZLE_2_MIN && x <= PUZZLE_2_MAX && y >= PUZZLE_2_MIN && y <= PUZZLE_2_MAX

// --- Structs ---
struct Beacon { long x; long y; };
struct Sensor { long x; long y; long range; };

// Checks if a position is in range of at least one sensor
bool is_in_range(struct Sensor* sensors, int len, long x, long y) {
	int i; for (i = 0; i < len; i++) {
		if (labs(sensors[i].x - x) + labs(sensors[i].y - y) <= sensors[i].range) return true;
	}
	return false;
}

// Checks if a position is empty
bool is_empty(struct Sensor* sensors, struct Beacon* beacons, int len, long x, long y) {
	int i; for (i = 0; i < len; i++) {
		if (sensors[i].x == x && sensors[i].y == y) return false;
		if (beacons[i].x == x && beacons[i].y == y) return false;
	}
	return true;
}

// Calculate the frequency for puzzle 2
long calculate_freq(long x, long y) {
	return x * PUZZLE_2_MAX + y;
}

// --- Main function ---
int main(void) {
	// --- Variables ---
	READ_BY_LINE_HEAD(file_t)
	READ_BY_LINE_INIT(file, file_t, INPUT_FILE)
	char line[file.max];
	int count = file.rows;
	struct Beacon beacons[count];
	struct Sensor sensors[count];
	int i, s;
	long x, y, sx, sy, bx, by, r, min_x = LONG_MAX, max_x = 0, max_r = 0;

	// --- Read and parse the input file ---
	READ_BY_LINE_WHILE(file, line)
		sscanf(line, "Sensor at x=%ld, y=%ld: closest beacon is at x=%ld, y=%ld",
			&sx, &sy, &bx, &by);
		r = labs(sx - bx) + labs(sy - by);
		beacons[i].x = bx;
		beacons[i].y = by;
		sensors[i].x = sx;
		sensors[i].y = sy;
		sensors[i++].range = r;
		if (sx < min_x) min_x = sx; if (sx > max_x) max_x = sx; if (r > max_r) max_r = r;
	READ_BY_LINE_DONE(file)
	min_x -= max_r;
	max_x += max_r;

	// --- Puzzle 1 ---
	int total = 0;
	for (x = min_x; x <= max_x; x++) {
		if (is_in_range(sensors, count, x, PUZZLE_1_LINE) &&
			is_empty(sensors, beacons, count, x, PUZZLE_1_LINE)) total++;
	}
	printf("1. Position that cannot contain a beacon: %d\n", total);

	// --- Puzzle 2 ---
	long freq = 0;
	for (s = 0; s < count; s++) {
		for (i = 0; i <= sensors[s].range; i++) {
			// North -> East
			x = sensors[s].x + i;
			y = sensors[s].y - sensors[s].range + i - 1;
			if (IN_LIMIT(x, y) && !is_in_range(sensors, count, x, y)) {
				freq = calculate_freq(x, y); break;
			}

			// East -> South
			x = sensors[s].x - sensors[s].range - i + 1;
			y = sensors[s].y + i;
			if (IN_LIMIT(x, y) && !is_in_range(sensors, count, x, y)) {
				freq = calculate_freq(x, y); break;
			}

			// South -> West
			x = sensors[s].x - i;
			y = sensors[s].y + sensors[s].range - i + 1;
			if (IN_LIMIT(x, y) && !is_in_range(sensors, count, x, y)) {
				freq = calculate_freq(x, y); break;
			}

			// West -> North
			x = sensors[s].x - sensors[s].range + i - 1;
			y = sensors[s].y - i;
			if (IN_LIMIT(x, y) && !is_in_range(sensors, count, x, y)) {
				freq = calculate_freq(x, y); break;
			}
		}
		if (freq > 0) break;
	}
	printf("2. Frequency: %ld\n", freq);

	return EXIT_SUCCESS;
}
