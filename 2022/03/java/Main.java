import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class Main {
	private static final String INPUT_FILE = "../input/input.txt";

	/**
	 * Find the duplicated character on both arrays.
	 *
	 * @param a
	 * @param b
	 * @return The duplicated character.
	 */
	private static char findDuplicate(char[] a, char[] b) {
		return findDuplicate(a, b, null);
	}

	/**
	 * Find the duplicated character on the arrays.
	 *
	 * @param a
	 * @param b
	 * @param c
	 * @return The duplicated character.
	 */
	private static char findDuplicate(char[] a, char[] b, char[] c) {
		for (int i = 0; i < a.length; i++)
			for (int j = 0; j < b.length; j++)
				if (a[i] == b[j]) {
					if (c == null) return a[i];
					for (int k = 0; k < c.length; k++)
						if (b[j] == c[k])
							return c[k];
				}
		throw new RuntimeException("Duplicate char not found!");
	}

	/**
	 * Get the priority value of a character.
	 *
	 * @param c Character
	 * @return An int value
	 */
	public static int getPriority(char c) {
		if (c >= 'a' && c <= 'z') return c - 'a' + 1;
		if (c >= 'A' && c <= 'Z') return c - 'A' + 27;
		throw new RuntimeException("Illegal char: " + c);
	}

	public static void main(String[] args) throws IOException {
		// --- Puzzle: Part 1 ---
		try (Stream<String> stream = Files.lines(Paths.get(INPUT_FILE))) {
			int total = stream.map(line -> {
				int len = line.length() / 2;
				String c1 = line.substring(0, len);
				String c2 = line.substring(len);
				char c = findDuplicate(c1.toCharArray(), c2.toCharArray());
				return getPriority(c);
			}).reduce(0, (acc, p) -> acc + p);
			System.out.format("Part 1. Total sum of the priorities: %,d%n", total);
		}

		// --- Puzzle: Part 2 ---
		int total = 0;
		try (BufferedReader reader = new BufferedReader(new FileReader(INPUT_FILE))) {
			String line;
			while((line = reader.readLine()) != null) {
				char[] a = line.toCharArray();
				char[] b = reader.readLine().toCharArray();
				char[] c = reader.readLine().toCharArray();
				char d = findDuplicate(a, b, c);
				total += getPriority(d);
			}
			System.out.format("Part 2. Total sum of the item types: %,d%n", total);
		}
	}
}
