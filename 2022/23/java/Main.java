import java.io.IOException;

public class Main {
	private static final String INPUT_FILE = "../input/input.txt";

	public static void main(String[] args) throws IOException {
		Grove grove = new Grove(INPUT_FILE);
		int round = 1;
		while(grove.sim()) {
			if (round++ == 10) {
				int total = grove.emptyCount();
				System.out.format("1. Empty groud tiles: %,d%n", total);
			}
		}
		System.out.format("2. First round with no Elf moves: %,d %n", round);
	}
}
