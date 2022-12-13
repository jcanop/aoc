import java.io.IOException;
import java.util.List;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";

	public static void main(String[] args) throws IOException {
		// --- Parse the input file ---
		Map map = Map.loadFromFile(INPUT_FILE);

		// --- Puzzle 1 ---
		List<Point> list =  map.findPath();
		int count = list.size();
		System.out.format("1. Fewest steps: %,d%n", count);

		// --- Puzzle 2 ---
		List<List<Point>> lists = map.findAllPaths();
		int min = lists.stream().map(l -> l.size()).min(Integer::compare).get();
		System.out.format("2. Shortest path: %,d%n", min);
	}
}
