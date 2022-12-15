import java.io.IOException;

public class Main {
	private static final String INPUT_FILE = "../input/input.txt";

	public static void main(String[] args) throws IOException {
		Map map = Map.load(INPUT_FILE);
		map.simulate();
		System.out.println(map.toString());
		long total = map.countSandTiles();
		System.out.format("1. Total units of sand: %,d%n", total);
	}
}
