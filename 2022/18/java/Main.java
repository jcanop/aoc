import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";

	// --- Searches for a path to the edge of the model ---
	private static boolean dfs(Point root, int[][][] data, Set<Point> closed) {
		List<Point> list = new ArrayList<>();
		Set<Point> visited = new HashSet<>();
		list.add(root);
		visited.add(root);

		while (list.size() > 0) {
			Point p = list.remove(0);
			if (p.x == 0 || p.x == data.length - 1 ||
				p.y == 0 || p.y == data[0].length - 1 ||
				p.z == 0 || p.z == data[0][0].length - 1) return true;

			if (data[p.x - 1][p.y][p.z] == 0) { Point n = p.from(-1, 0, 0); if (!visited.contains(n)) { list.add(n); visited.add(n); }}
			if (data[p.x + 1][p.y][p.z] == 0) { Point n = p.from( 1, 0, 0); if (!visited.contains(n)) { list.add(n); visited.add(n); }}
			if (data[p.x][p.y - 1][p.z] == 0) { Point n = p.from(0, -1, 0); if (!visited.contains(n)) { list.add(n); visited.add(n); }}
			if (data[p.x][p.y + 1][p.z] == 0) { Point n = p.from(0,  1, 0); if (!visited.contains(n)) { list.add(n); visited.add(n); }}
			if (data[p.x][p.y][p.z - 1] == 0) { Point n = p.from(0, 0, -1); if (!visited.contains(n)) { list.add(n); visited.add(n); }}
			if (data[p.x][p.y][p.z + 1] == 0) { Point n = p.from(0, 0,  1); if (!visited.contains(n)) { list.add(n); visited.add(n); }}
		}

		closed.addAll(visited);
		return false;
	}

	// --- Calculates the surface ---
	private static int calculate(int[][][] data, List<int[]> list) {
		int total = 0;
		for (int[] p: list) {
			int x = p[0];
			int y = p[1];
			int z = p[2];
			long n = 6;
			if (x > 0 && data[x - 1][y][z] == 1) n--;
			if (x < data.length - 1 && data[x + 1][y][z] == 1) n--;
			if (y > 0 && data[x][y - 1][z] == 1) n--;
			if (y < data[0].length - 1 && data[x][y + 1][z] == 1) n--;
			if (z > 0 && data[x][y][z - 1] == 1) n--;
			if (z < data[0][0].length - 1 && data[x][y][z + 1] == 1) n--;
			total += n;
		}
		return total;
	}

	// --- Main method ---
	public static void main(String[] args) throws IOException {
		List<int[]> list = new ArrayList<>();
		int[] max = new int[3];

		// --- Read and parse the input file ---
		try (BufferedReader reader = new BufferedReader(new FileReader(INPUT_FILE))) {
			String line;
			while ((line = reader.readLine()) != null) {
				int[] a = Arrays.stream(line.split(",")).mapToInt(Integer::parseInt).toArray();
				for (int i = 0; i < 3; i++) max[i] = Math.max(max[i], a[i]);
				list.add(a);
			}
		}
		int[][][] data = new int[max[0] + 1][max[1] + 1][max[2] + 1];
		for (int[] a: list) {
			data[a[0]][a[1]][a[2]] = 1;
		}

		// --- Puzzle 1 ---
		int total = calculate(data, list);
		System.out.format("1. Total surface: %,d%n", total);

		// --- Puzzle 2 ---
		Set<Point> closed = new HashSet<>();
		for (int x = 1; x < data.length - 1; x++) {
			for (int y = 1; y < data[0].length - 1; y++) {
				for (int z = 1; z < data[0][0].length - 1; z++) {
					if (data[x][y][z] == 1) continue;
					Point p = new Point(x, y, z);
					if (!closed.contains(p)) dfs(p, data, closed);
				}
			}
		}
		for (Point p: closed) data[p.x][p.y][p.z] = 1;

		total = calculate(data, list);
		System.out.format("2. Adjusted total surface: %,d%n", total);
	}
}
