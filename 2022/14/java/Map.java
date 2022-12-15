import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.HashMap;

public class Map {
	private static final int OFFSET = 2;
	private static final Point ORIGIN  = new Point(500, 0);

	private final HashMap<Point, Tile> data;
	private final int height;
	private final Point pointer;

	private Map(HashMap<Point,Tile> data, int height) {
		this.data = data;
		this.height = height;
		this.pointer = new Point(0, 0);
	}

	public static Map load(String filename) throws IOException {
		List<List<Point>> list = new ArrayList<>();
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
					maxy = Math.max(y, maxy);
				}
				list.add(ln);
			}
		}

		HashMap<Point, Tile> data = new HashMap<>();
		int h = maxy + 3;
		for (List<Point> ln:  list) {
			for (int i = 1; i < ln.size(); i++) {
				Point p1 = ln.get(i -1);
				Point p2 = ln.get(i);
				int px1 = p1.x;
				int py1 = p1.y;
				int px2 = p2.x;
				int py2 = p2.y;
				int x1 = Math.min(px1, px2);
				int x2 = Math.max(px1, px2);
				int y1 = Math.min(py1, py2);
				int y2 = Math.max(py1, py2);
				for (int y = y1; y <= y2; y++)
					for (int x = x1; x <= x2; x++)
						data.put(new Point(x, y), Tile.ROCK);
			}
		}

		return new Map(data, h);
	}

	public Tile get(int x, int y) {
		if (x == ORIGIN.x && y == ORIGIN.y) return Tile.ORIGIN;
		pointer.x = x;
		pointer.y = y;
		Tile tile = data.get(pointer);
		if (tile == null) return Tile.AIR;
		return tile;
	}

	public long countSandTiles() {
		return data.values().stream().filter(x -> x == Tile.SAND).count();
	}

	public void simulate() {
		while (true) {
			int x = ORIGIN.x;
			int y = ORIGIN.y;
			while (true) {
	//System.out.println("=> " + x + ", " + y);
				if (y + 1 == height + 1) return;
				if (get(x, y + 1) == Tile.AIR) { y++; continue; }
				if (get(x - 1, y + 1) == Tile.AIR) { y++; x--; continue; }
				if (get(x + 1, y + 1) == Tile.AIR) { y++; x++; continue; }
				data.put(new Point(x, y), Tile.SAND);
				break;
			}
		}
	}

	@Override
	public String toString() {
		int min = Integer.MAX_VALUE;
		int max = 0;
		for (Point p: data.keySet()) {
			min = Math.min(min, p.x);
			max = Math.max(max, p.x);
		}
		min -= OFFSET;
		max += OFFSET + 1;

		StringBuilder sb = new StringBuilder();
		for (int y = 0; y < height; y++) {
			sb.append('|');
			for (int x = min; x < max; x++) {
				if (y < height - 1) sb.append(get(x, y).display());
				else sb.append(Tile.ROCK.display());
			}
			sb.append("|\n");
		}
		return sb.toString();
	}
}
