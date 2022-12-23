import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Map {
	// --- Constants ---
	private static final char EMPTY = ' ';
	private static final char OPEN = '.';
	private static final char WALL = '#';
	private static final char NORTH = '^';
	private static final char EAST = '>';
	private static final char SOUTH = 'V';
	private static final char WEST = '<';
	private static final char RIGHT = 'R';
	private static final char LEFT = 'L';

	private final char[][] grid;
	private final int height;
	private final int width;
	private Point pointer;
	private char direction;
	private char[][] path;

	public Map(String data) {
		String[] lines = data.split("\\r?\\n");
		this.height = lines.length;
		this.width = Arrays.stream(lines).mapToInt(line -> line.length()).max().getAsInt();
		this.grid = new char[this.height][this.width];
		this.direction = EAST;

		for (int y = 0; y < lines.length; y++) {
			char[] line = lines[y].toCharArray();
			for (int x = 0; x < line.length; x++) {
				this.grid[y][x] = line[x];
			}
			for (int x = line.length; x < this.width; x++) {
				this.grid[y][x] = EMPTY;
			}
		}

		for (int y = 0; y < height; y++) {
			for (int x = 0; x < width; x++) {
				if (grid[y][x] == OPEN) {
					this.pointer = new Point(x, y);
					break;
				}
			}
			if (pointer != null) break;
		}
	}

	private void move(int steps) {
		int x = pointer.x;
		int y = pointer.y;
		while (steps > 0) {
			switch (direction) {
				case NORTH: y--; if (y < 0) y = height - 1;  break;
				case SOUTH: y++; if (y == height - 1) y = 0; break;
				case WEST: x--;  if (x < 0) x = width -1 ;   break;
				case EAST: x++;  if (x == width - 1) x = 0;  break;
				default: throw new IllegalArgumentException("" +direction);
			}
			if (grid[y][x] == WALL) return;
			if (grid[y][x] == OPEN) {
				steps--;
				pointer.x = x;
				pointer.y = y;
				path[y][x] = direction;
			}
		}
	}

	public void path(String s) {
		path = new char[height][width];
		for (int y = 0; y < height; y++) Arrays.fill(path[y], EMPTY);

		Pattern pattern = Pattern.compile("(\\d+|R|L)");
		Matcher matcher = pattern.matcher(s);
		while (matcher.find()) {
			String token = matcher.group(1);
			if (token.matches("\\d+")) {
				int steps = Integer.parseInt(token);
				move(steps);
			} else {
				char turn = token.charAt(0);
				switch (direction) {
					case NORTH: direction = turn == RIGHT ? EAST  : WEST;  break;
					case EAST:  direction = turn == RIGHT ? SOUTH : NORTH; break;
					case SOUTH: direction = turn == RIGHT ? WEST  : EAST;  break;
					case WEST:  direction = turn == RIGHT ? NORTH : SOUTH; break;
					default: throw new IllegalArgumentException("" +direction);
				}
			}
		}
	}

	public long getPassword() {
		long row = pointer.y + 1;
		long col = pointer.x + 1;
		long result = 1_000 * row + 4 * col;
		switch(direction) {
			case NORTH: result += 3; break;
			case EAST:  break;
			case SOUTH: result += 1; break;
			case WEST:  result += 2; break;
			default: throw new IllegalArgumentException("" +direction);
		}
		return result;
	}

	public void print() {
		for (int y = 0; y < height; y++) {
			for (int x = 0; x < width; x++) {
				char a = grid[y][x];
				char b = path[y][x];
				if (b != EMPTY) System.out.print(path[y][x]);
				else System.out.print(grid[y][x]);
			}
			System.out.println();
		}
	}
}
