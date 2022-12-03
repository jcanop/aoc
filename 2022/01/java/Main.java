import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Main {
	private static final String INPUT_FILE = "../input/input.txt";

	public static void main(String[] args) throws IOException {
		List<Long> list = new ArrayList<>();
		long total = 0;

		// --- Read and parse the input file ---
		try (BufferedReader reader = new BufferedReader(new FileReader(INPUT_FILE))) {
			String line;
			while ((line = reader.readLine()) != null) {
				line = line.trim();
				if (line.length() == 0) {
					list.add(total);
					total = 0;
				} else {
					total += Long.parseLong(line);
				}
			}
			list.add(total);
		}
		Collections.sort(list, Collections.reverseOrder());

		// --- Find the Elf carrying the most Calories ---
		long max = list.get(0);
		System.out.format("The Elf carrying the most Calories, is carrying %,d Calories.%n", max);

		// --- Find the top three Elves carrying the most Calories ---
		total = list.stream().limit(3).reduce(0L, Long::sum);
		System.out.format("The top 3 Elves carrying the most Calores, are carrying %,d Calories.%n", total);
	}
}
