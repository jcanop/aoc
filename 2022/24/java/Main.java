import java.io.IOException;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";

	public static void main(String[] args) throws IOException {
		// --- Puzzle 1 ---
		Map map = new Map(INPUT_FILE);
		Point start = new Point(1, 0);
		Point end = new Point(map.getWidth() - 2, map.getHeight() - 1);
		int time = map.search(start, end, 0);
		System.out.format("1. Minutes to reach the goal: %,d%n", time);

		// --- Puzzle 2 ---
		time = map.search(end, start, time);
		time = map.search(start, end, time);
		System.out.format("2. Minutes to reach the goal, go back to the start, then reach the goal again: %,d%n", time);
	}
}
