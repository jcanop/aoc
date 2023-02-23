#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <map.h>

void move_grid(Map* map, int steps) {
	int x = map->x;
	int y = map->y;

	while (steps > 0) {
		switch(map->d) {
			case NORTH: y = (y == 0) ? M_HEIGHT - 1 : y - 1; break;
			case SOUTH: y = (y == M_HEIGHT - 1) ? 0 : y + 1; break;
			case WEST:  x = (x == 0) ? M_WIDTH - 1  : x - 1; break;
			case EAST:  x = (x == M_WIDTH - 1)  ? 0 : x + 1; break;
			default:
				fprintf(stderr, "Direction unsupported: %d\n", map->d);
				exit(EXIT_FAILURE);
		}
		if (map->grid[y][x] == WALL) break;
		if (map->grid[y][x] == OPEN) {
			steps--;
			map->x = x;
			map->y = y;
		}
	}
}

void move_cube(Map* map, int steps) {
	while (steps > 0) {
		int x = map->x;
		int y = map->y;
		int d = map->d;

		switch (map->d) {
			case NORTH: y--; break;
			case SOUTH: y++; break;
			case WEST:  x--; break;
			case EAST:  x++; break;
			default: goto unreachable;
		}

		if (x < 0 || x >= M_WIDTH || y < 0 || y >= M_HEIGHT || map->grid[y][x] == EMPTY) {
			int r = floor(map->y / 50);
			int c = floor(map->x / 50);

			if      (r == 0 && c == 1 && d == NORTH) { r = 3; c = 0; d = EAST;  }
			else if (r == 0 && c == 1 && d == WEST)  { r = 2; c = 0; d = EAST;  }
			else if (r == 0 && c == 2 && d == NORTH) { r = 3; c = 0; d = NORTH; }
			else if (r == 0 && c == 2 && d == EAST)  { r = 2; c = 1; d = WEST;  }
			else if (r == 0 && c == 2 && d == SOUTH) { r = 1; c = 1; d = WEST;  }
			else if (r == 1 && c == 1 && d == EAST)  { r = 0; c = 2; d = NORTH; }
			else if (r == 1 && c == 1 && d == WEST)  { r = 2; c = 0; d = SOUTH; }
			else if (r == 2 && c == 0 && d == NORTH) { r = 1; c = 1; d = EAST;  }
			else if (r == 2 && c == 0 && d == WEST)  { r = 0; c = 1; d = EAST;  }
			else if (r == 2 && c == 1 && d == EAST)  { r = 0; c = 2; d = WEST;  }
			else if (r == 2 && c == 1 && d == SOUTH) { r = 3; c = 0; d = WEST;  }
			else if (r == 3 && c == 0 && d == EAST)  { r = 2; c = 1; d = NORTH; }
			else if (r == 3 && c == 0 && d == SOUTH) { r = 0; c = 2; d = SOUTH; }
			else if (r == 3 && c == 0 && d == WEST)  { r = 0; c = 1; d = SOUTH; }
			else goto unreachable;

			int ci = map->x % 50;
			int ri = map->y % 50;

			int i = -1;
			switch (map->d) {
				case NORTH: i = ci; break;
				case SOUTH: i = 49 - ci; break;
				case WEST:  i = 49 - ri; break;
				case EAST:  i = ri; break;
				default: goto unreachable;
			}

			int rn = -1, cn = -1;
			switch (d) {
				case NORTH: rn = 49; cn = i; break;
				case SOUTH: rn = 0; cn = 49 - i; break;
				case WEST:  rn = 49 - i; cn = 49; break;
				case EAST:  rn = i; cn = 0; break;
				default: goto unreachable;
			}

			x = 50 * c + cn;
			y = 50 * r + rn;
		}

		if (map->grid[y][x] == WALL) break;
		if (map->grid[y][x] == OPEN) {
			steps--;
			map->x = x;
			map->y = y;
			map->d = d;
		}
	}
	return;

unreachable:
	fprintf(stderr, "Unreachable code!\n");
}
