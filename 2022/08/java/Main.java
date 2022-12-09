import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";

	/**
	 * Checks if a tree is visible.
	 */
	private static boolean isVisible(int[][] grid, int x, int y) {
		for (int i = y + 1; i < grid.length; i++) {
			if (grid[i][x] >= grid[y][x]) break;
			if (i == grid.length - 1) return true;
		}
		for (int i = y - 1; i >= 0; i--) {
			if (grid[i][x] >= grid[y][x]) break;
			if (i == 0) return true;
		}
		for (int i = x + 1; i < grid[0].length; i++) {
			if (grid[y][i] >= grid[y][x]) break;
			if (i == grid[0].length - 1) return true;
		}
		for (int i = x - 1; i >= 0; i--) {
			if (grid[y][i] >= grid[y][x]) return false;
			if (i == 0) return true;
		}
		throw new RuntimeException("Unreachable code!");
	}

	/**
	 * Calculates the scenic score of a tree.
	 */
	private static int scenicScore(int[][] grid, int x, int y) {
		int a = 1;
		for (int i = y + 1; i < grid.length - 1; i++) {
			if (grid[i][x] >= grid[y][x]) break;
			a++;
		}
		int b = 1;
		for (int i = y - 1; i > 0; i--) {
			if (grid[i][x] >= grid[y][x]) break;
			b++;
		}
		int c = 1;
		for (int i = x + 1; i < grid[0].length - 1; i++) {
			if (grid[y][i] >= grid[y][x]) break;
			c++;
		}
		int d = 1;
		for (int i = x - 1; i > 0; i--) {
			if (grid[y][i] >= grid[y][x]) break;
			d++;
		}
		return a * b * c * d;
	}

	public static void main(String[] args) throws IOException {
		// --- Reads and parse the input file ---
		List<String> lines = Files.readAllLines(Paths.get(INPUT_FILE));
		int[][] grid = new int[lines.size()][];
		for (int i = 0; i < lines.size(); i++) {
			grid[i] = lines.get(i).chars().map(Integer::valueOf).toArray();
		}

		// --- Puzzle 1 ---
		int total = (grid.length + grid[0].length - 2) * 2;
		for (int y = 1; y < grid.length - 1; y++)
			for (int x = 1; x < grid[0].length - 1; x++)
				total += isVisible(grid, x, y) ? 1 : 0;
		System.out.format("1. Trees that are visible from aoutside the grid: %,d%n", total);

		// --- Puzzle 2 ---
		int max = 0;
		for (int y = 1; y < grid.length - 1; y++)
			for (int x = 1; x < grid[0].length - 1; x++) {
				int score = scenicScore(grid, x, y);
				if (score > max) max = score;
			}
		System.out.format("2. The highest scenic score is:  %,d%n", max);
	}
}
