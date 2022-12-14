import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Main {
	// --- Constant ---
	private static final String INPUT_FILE = "../input/input.txt";

	public static void main(String[] args) throws IOException {
		List<ArrayItem> list = new ArrayList<>();
		int total = 0;

		// --- Read and parse the input file ---
		try (BufferedReader reader = new BufferedReader(new FileReader(INPUT_FILE))) {
			ArrayItem a = null;
			ArrayItem b = null;
			int index = 0;
			String line;
			while ((line = reader.readLine()) != null) {
				int id = index / 3 + 1;
				switch (index++ % 3) {
					case 0: a = new ArrayItem(line); list.add(a); break;
					case 1: b = new ArrayItem(line); list.add(b);
						if (a.compareTo(b) < 0) total += id;
						break;
				}
			}
		}

		// --- Puzzle 1 ---
		System.out.format("1. The sum of the indices of the right order pairs: %,d%n", total);

		// --- Puzzle 2 ---
		ArrayItem divider1 = new ArrayItem("[[2]]");
		ArrayItem divider2 = new ArrayItem("[[6]]");
		list.add(divider1);
		list.add(divider2);
		Collections.sort(list);
		int index1 = list.indexOf(divider1) + 1;
		int index2 = list.indexOf(divider2) + 1;
		total = index1 * index2;
		System.out.format("2. The decoder key: %,d%n", total);
	}
}
