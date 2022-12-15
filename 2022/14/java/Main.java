import java.io.IOException;

public class Main {
	private static final String INPUT_FILE = "../input/input.txt";

	public static void main(String[] args) throws IOException {
		Map map = Map.load(INPUT_FILE);
		int total = map.simulate();
		System.out.println(map.toString());
		System.out.format("1. Total units of sand: %,d%n", total);
	}
}
