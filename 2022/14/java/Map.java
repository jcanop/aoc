import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Map {
	private final Tile[][] grid;
	private final int height;
	private final int width;
	private final Point origin;

	private Map(Tile[][] grid, int height, int width, Point origin) {
		this.grid = grid;
		this.height = height;
		this.width = width;
		this.origin = origin;
	}

	public static Map load(String filename) throws IOException {
		List<List<Point>> list = new ArrayList<>();
		int minx = Integer.MAX_VALUE;
		int maxx = 0;
		int maxy = 0;

		try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
			String line;
			while ((line = reader.readLine()) != null) {
				List<Point> ln = new ArrayList<>();
				for (String token: line.split("\\s*->\\s*")) {
					String s[] = token.split("\\s*,\\s*");
					int x = Integer.parseInt(s[0]);
					int y = Integer.parseInt(s[1]);
					ln.add(new Point(x, y));

					minx = Math.min(x, minx);
					maxx = Math.max(x, maxx);
					maxy = Math.max(y, maxy);
				}
				list.add(ln);
			}
		}

		int h = maxy + 1;
		int w = maxx - minx + 3;
		Tile[][] grid = new Tile[h][w];
		for (int y = 0; y < h; y++) for (int x = 0; x < w; x++) grid[y][x] = Tile.AIR;

		for (List<Point> ln:  list) {
			for (int i = 1; i < ln.size(); i++) {
				Point p1 = ln.get(i -1);
				Point p2 = ln.get(i);
				int px1 = p1.getX() - minx + 1;
				int py1 = p1.getY();
				int px2 = p2.getX() - minx + 1;
				int py2 = p2.getY();
				int x1 = Math.min(px1, px2);
				int x2 = Math.max(px1, px2);
				int y1 = Math.min(py1, py2);
				int y2 = Math.max(py1, py2);
				for (int y = y1; y <= y2; y++) for (int x = x1; x <= x2; x++) grid[y][x] = Tile.ROCK;
			}
		}

		Point o = new Point(500 - minx + 1, 0);
		grid[o.getY()][o.getX()] = Tile.ORIGIN;
		return new Map(grid, h, w, o);
	}

	public int simulate() {
		int count = 0;
		while (true) {
			int x = origin.getX();
			int y = origin.getY();
			while (true) {
	//System.out.println("=> " + x + ", " + y);
				if (y + 1 == height) return count;
				if (grid[y + 1][x] == Tile.AIR) { y++; continue; }
				if (grid[y + 1][x - 1] == Tile.AIR) { y++; x--; continue; }
				if (grid[y + 1][x + 1] == Tile.AIR) { y++; x++; continue; }
				grid[y][x] = Tile.SAND;
				break;
			}
			count++;
		}
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (int y = 0; y < height; y++) {
			sb.append('|');
			for (int x = 0; x < width; x++) {
				sb.append(grid[y][x].display());
			}
			sb.append("|\n");
		}
		return sb.toString();
	}

}
