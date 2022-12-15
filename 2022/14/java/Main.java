import java.io.IOException;

public class Main {
	private static final String INPUT_FILE = "../input/input.txt";

	public static void main(String[] args) throws IOException {
		// --- Read and parse the input file ---
		Map map = Map.load(INPUT_FILE);

		// --- Puzzle 1 ---
		map.simulate(1);
		long total = map.countSandTiles();
		System.out.format("1. Total units of sand: %,d%n", total);

		// --- Puzzle 2 ---
		map.simulate(2);
		total = map.countSandTiles();
		System.out.format("2. Total units of sand: %,d%n", total);
	}
}
