/**
 * Harcoded Cube Map for the input file :-(
 *
 *         0        1       2
 *              +-------+-------+
 *              |   g   |   e   |
 * 0            |f      |      d|
 *              |       |   b   |
 *              +-------+-------+
 *              |       |
 * 1            |a     b|
 *              |       |
 *      +-------+-------+
 *      |   a   |       |
 * 2    |f      |      d|
 *      |       |   c   |
 *      +-------+-------+
 *      |       |
 * 3    |g     c|
 *      |   e   |
 *      +-------+
 */
public class Cube50Map extends Map {
	public static final String ANSI_YELLOW = "\u001B[33m";
	public static final String ANSI_RESET = "\u001B[0m";
	private static final int SIZE = 50;

	public Cube50Map(String data) {
		super(data);
	}

	@Override
	protected void move(int steps) {
		while (steps > 0) {
			int x = pointer.x;
			int y = pointer.y;
			char d = direction;
			switch (direction) {
				case NORTH: y--; break;
				case SOUTH: y++; break;
				case WEST:  x--; break;
				case EAST:  x++; break;
				default: throw new RuntimeException("Unreachable!");
			}

			if (x < 0 || y < 0 || x >= width || y >= height || grid[y][x] == EMPTY) {
				int r = pointer.y / SIZE;
				int c = pointer.x / SIZE;
				d = direction;
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
				else throw new RuntimeException("Unreachable!");

				int ci = pointer.x % SIZE;
				int ri = pointer.y % SIZE;

				int i = -1;
				switch (direction) {
					case NORTH: i = ci; break;
					case SOUTH: i = SIZE - 1 - ci; break;
					case WEST:  i = SIZE - 1 - ri; break;
					case EAST:  i = ri; break;
					default: throw new RuntimeException("Unreachable!");
				}

				int rn = -1, cn = -1;
				switch (d) {
					case NORTH: rn = SIZE - 1;     cn = i;            break;
					case SOUTH: rn = 0;            cn = SIZE - 1 - i; break;
					case WEST:  rn = SIZE - 1 - i; cn = SIZE - 1;     break;
					case EAST:  rn = i;            cn = 0;            break;
					default: throw new RuntimeException("Unreachable!");
				}

				x = c * SIZE + cn;
				y = r * SIZE + rn;
			}

			if (grid[y][x] == WALL) return;
			if (grid[y][x] == OPEN) {
				steps--;
				path[y][x] = d;
				pointer.x = x;
				pointer.y = y;
				direction = d;
			} else throw new RuntimeException("Empty space!: " + x + ", " + y);
		}
	}

	@Override
	public void print() {
		for (int y = 0; y < height; y++) {
			if (y > 0 && y % SIZE == 0) {
				for (int x = 0; x < width; x++) {
					if (x > 0 && x % SIZE == 0) System.out.print("+");
					System.out.print("-");
					if (x == width - 1) System.out.println();
				}
			}
			for (int x = 0; x < width; x++) {
				if (x > 0 && x % SIZE == 0) System.out.print("|");
				char a = grid[y][x];
				char b = path == null ? EMPTY : path[y][x];
				if (b != EMPTY) System.out.print(ANSI_YELLOW + path[y][x] + ANSI_RESET);
				else System.out.print(grid[y][x]);
			}
			System.out.println();
		}
	}
}
