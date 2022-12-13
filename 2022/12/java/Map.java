import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Represents the Map.
 */
public class Map {
	private char[][] grid;
	private Point start;
	private Point end;

	private Map(char[][] grid, Point start, Point end) {
		this.grid = grid;
		this.start = start;
		this.end = end;
	}

	/**
	 * Loads the Map from the file.
	 *
	 * @param filename Filename to load.
	 * @throws IOExeption When an I/O error ocurrs.
	 */
	public static Map loadFromFile(String filename) throws IOException {
		char[][] grid = null;
		Point start = null;
		Point end = null;
		try (Stream<String> stream = Files.lines(Paths.get(filename))) {
			List<String> lines = stream.collect(Collectors.toList());
			grid = new char[lines.size()][lines.get(0).length()];
			for (int y = 0; y < lines.size(); y++) {
				char[] chars  = lines.get(y).toCharArray();
				for (int x = 0; x < chars.length; x++) {
					char c = chars[x];
					switch (c) {
						case 'S': grid[y][x] = 'a'; start = new Point(x, y); break;
						case 'E': grid[y][x] = 'z'; end = new Point(x, y); break;
						default: grid[y][x] = c;
					}
				}
			}
		}
		if (grid == null) throw new NullPointerException("Grid can be null");
		if (start == null) throw new NullPointerException("Start can be null");
		if (end == null) throw new NullPointerException("End can be null");
		return new Map(grid, start, end);
	}

	/**
	 * Find the shorts path.
	 *
	 * @return The list of point that describe the path.
	 */
	public List<Point> findPath() {
		int h = grid.length;
		int w = grid[0].length;
		int x = start.getX();
		int y = start.getY();
		int[][] a = new int[h][w];
		a[y][x] = 1;
		List<Point> list = new ArrayList<>();
		list.add(new Point(x, y));
		while (list.size() > 0) {
			Point p = list.remove(0);
			x = p.getX();
			y = p.getY();
			if (x > 0     && a[y][x - 1] == 0  && grid[y][x - 1] <= grid[y][x] + 1) { a[y][x - 1] = a[y][x] + 1; list.add(new Point(x - 1, y)); }
			if (y > 0     && a[y - 1][x] == 0  && grid[y - 1][x] <= grid[y][x] + 1) { a[y - 1][x] = a[y][x] + 1; list.add(new Point(x, y - 1)); }
			if (x < w - 1 && a[y][x + 1] == 0  && grid[y][x + 1] <= grid[y][x] + 1) { a[y][x + 1] = a[y][x] + 1; list.add(new Point(x + 1, y)); }
			if (y < h - 1 && a[y + 1][x] == 0  && grid[y + 1][x] <= grid[y][x] + 1) { a[y + 1][x] = a[y][x] + 1; list.add(new Point(x, y + 1)); }
		}

		x = end.getX();
		y = end.getY();
		int c = a[y][x];
		list = new ArrayList<Point>();
		while (c-- > 1) {
			if (x > 0 && a[y][x - 1] == c) x--;
			else if (y > 0 && a[y - 1][x] == c) y--;
			else if (x < w - 1 && a[y][x + 1] == c) x++;
			else if (y < h - 1 && a[y + 1][x] == c) y++;
			list.add(new Point(x, y));
		}
		return list;
	}

	/**
	 * Find all the shortest paths from the lowest points of the maps.
	 *
	 * @return The list of paths.
	 */
	public List<List<Point>> findAllPaths() {
		int h = grid.length;
		int w = grid[0].length;
		List<List<Point>> list = new ArrayList<>();
		for (int y = 0; y < h; y++) {
			for (int x = 0; x < w; x++) {
				if (grid[y][x] == 'a') {
					start = new Point(x, y);
					List<Point> path = findPath();
					if (path.size() > 0) list.add(findPath());
				}
			}
		}
		return list;
	}
}
