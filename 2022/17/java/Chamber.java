import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;

/**
 * Represents the chamber with the falling rocks
 */
public class Chamber {
	// --- Constants ---
	private static final int WIDTH = 7;
	private static final int MARGIN_LEFT = 2;
	private static final int MARGIN_TOP = 3;
	private static final char AIR = '.';
	private static final char ROCK = '@';
	private static final char SAND = '#';
	private static final char LEFT = '<';
	private static final char RIGHT = '>';
	private static final char DOWN = 'V';

	private final List<char[]> rows;
	private int[][] shape;

	public Chamber() {
		this.rows = new ArrayList<>();
	}

	public long getHeight() {
		return (long) (findFromTop(SAND) - 1);
	}

	/**
	 * Private method that searches ther first appearance of a type from the top.
	 *
	 * @param c Type to search.
	 * @return The row of the first appearance or -1 if not found.
	 */
	private int findFromTop(char c) {
		for (int y = rows.size() - 1; y >= 0; y--) {
			char[] row = rows.get(y);
			for (int x = 0; x < WIDTH; x++) {
				if (row[x] == c) return y;
			}
		}
		return -1;
	}

	/**
	 * Adds a new Rock Shape at the top.
	 *
	 * @param shape Rock shape.
	 */
	public void addShape(int[][] shape) {
		this.shape = shape;

		// --- Remove extra rows ---
		int top = findFromTop(SAND);
		if (top >= 0) {
			int n = rows.size() - top - 1;
			for (int i = 0; i < n; i++) rows.remove(rows.size() - 1);
		}

		// --- Add or remove rows if needed --
		for (int i = 0; i < MARGIN_TOP + shape.length; i++) {
			char[] row = new char[WIDTH];
			Arrays.fill(row, AIR);
			this.rows.add(row);
		}

		// --- Add the shape ---
		int height = rows.size() - 1;
		for (int y = 0; y < shape.length; y++) {
			char[] row = rows.get(height - y);
			for (int x: shape[y]) {
				row[MARGIN_LEFT + x] = ROCK;
			}
		}
	}

	/**
	 * Test if a shape can move in one direction.
	 *
	 * @param direction Direction to move.
	 * @return true if the shape can move in that direction, otherwise false.
	 */
	private boolean canMove(char direction) {
		int i = findFromTop(ROCK);
		if (i == -1)  throw new RuntimeException("Shape not found!");
		if (direction == DOWN) {
			i -= shape.length - 1;
			if (i == 0) return false;
			for (int y = i; y < i + shape.length; y++) {
				char[] up = rows.get(y);
				char[] down = rows.get(y - 1);
				for (int x = 0; x < WIDTH; x++) {
					if (up[x] == ROCK && down[x] == SAND) return false;
				}
			}
		} else if (direction == LEFT) {
			for (int y = i; y > i - shape.length; y--) {
				char[] row = rows.get(y);
				if (row[0] == ROCK) return false;
				for (int x = 1; x < WIDTH; x++) {
					if (row[x] == ROCK) {
						if (row[x - 1] != AIR) return false;
						break;
					}
				}
			}
		} else if (direction == RIGHT) {
			for (int y = i; y > i - shape.length; y--) {
				char[] row = rows.get(y);
				if (row[WIDTH - 1] == ROCK) return false;
				for (int x = WIDTH - 2; x >= 0; x--) {
					if (row[x] == ROCK) {
						if (row[x + 1] != AIR) return false;
						break;
					}
				}
			}
		} else {
			throw new RuntimeException("Unsupported direction: " + direction);
		}
		return true;
	}

	/**
	 * Moves the current rock shape down one step.
	 */
	public boolean down() {
		return move(DOWN);
	}

	/**
	 * Moves the current rock shape one step.
	 *
	 * @param direction Direction to move.
	 * @return true if the shape can still be moved, otherwise false.
	 */
	public boolean move(char direction) {
		if (canMove(direction)) {
			if (direction == DOWN) {
				int row = findFromTop(ROCK) - (shape.length - 1);
				for (int y = row; y < row + shape.length; y++) {
					char[] up = rows.get(y);
					char[] down = rows.get(y - 1);
					for (int x = 0; x < WIDTH; x++) {
						if (up[x] == ROCK) {
							down[x] = up[x];
							up[x] = AIR;
						}
					}
				}
			} else if (direction == LEFT) {
				int i = findFromTop(ROCK);
				for (int y = i; y > i - shape.length; y--) {
					char[] row = rows.get(y);
					for (int x = 0; x < WIDTH - 1; x++) {
						if (row[x + 1] == ROCK) {
							row[x] = row[x + 1];
							row[x + 1] = AIR;
						}
					}
				}
			} else if (direction == RIGHT) {
				int i = findFromTop(ROCK);
				for (int y = i; y > i - shape.length; y--) {
					char[] row = rows.get(y);
					for (int x = WIDTH - 1; x > 0; x--) {
						if (row[x - 1] == ROCK) {
							row[x] = row[x - 1];
							row[x - 1] = AIR;
						}
					}
				}
			} else {
				throw new RuntimeException("Unsupported direction: " + direction);
			}
		} else if (direction == DOWN) {
			int i = findFromTop(ROCK) - (shape.length - 1);
			for (int y = i; y < i + shape.length; y++) {
				char[] row = rows.get(y);
				for (int x = 0; x < WIDTH; x++) {
					if (row[x] == ROCK) row[x] = SAND;
				}
			}
			return false;
		}
		return true;
	}

	/**
	 * Prints the chmaber into the console for debuging.
	 */
	public void print() {
		for (int i = rows.size() - 1; i >= 0; i--) {
			System.out.print("|");
			for (char c: rows.get(i)) System.out.print(c);
			System.out.println("|");
		}
		System.out.print('+');
		for (int i = 0; i < WIDTH; i++) System.out.print('-');
		System.out.println('+');
	}
}
