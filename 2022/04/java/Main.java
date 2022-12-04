import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class Main {
	private static final String INPUT_FILE = "../input/input.txt";

	/**
	 * Private class that represents a range.
	 */
	private static class Range {
		private final int min;
		private final int max;

		/**
		 * Constructor
		 *
		 * @param value String value to parse.
		 */
		private Range(String value) {
			String[] tokens = value.split("-");
			this.min = Integer.parseInt(tokens[0]);
			this.max = Integer.parseInt(tokens[1]);
		}

		/**
		 * Checks if this range fully contains another range.
		 *
		 * @param r Another range.
		 * @return A boolean
		 */
		private boolean fullyContains(Range r) {
			return this.min <= r.min && r.max <= this.max;
		}

		/**
		 * Checks if this range overlaps with another range.
		 *
		 * @param r Another range.
		 * @return A boolean
		 */
		private boolean overlap(Range r) {
			return
				(r.min >= this.min && r.min <= this.max) ||
				(r.max >= this.min && r.max <= this.max) ||
				(this.min >= r.min && this.min <= r.max) ||
				(this.max >= r.min && this.max <= r.max);
		}
	}

	public static void main(String[] args) throws IOException {
		// --- Puzzle 1 ---
		try (Stream<String> stream = Files.lines(Paths.get(INPUT_FILE))) {
			int total = stream.mapToInt(line -> {
				String[] tokens = line.split(",");
				Range r1 = new Range(tokens[0]);
				Range r2 = new Range(tokens[1]);
				return r1.fullyContains(r2) || r2.fullyContains(r1) ? 1 : 0;
			}).sum();
			System.out.format("Part 1. Total of ranges that fully contains another: %,d%n", total);
		}

		// -- Puzzle 2 ---
		try (Stream<String> stream = Files.lines(Paths.get(INPUT_FILE))) {
			int total = stream.mapToInt(line -> {
				String[] tokens = line.split(",");
				Range r1 = new Range(tokens[0]);
				Range r2 = new Range(tokens[1]);
				return r1.overlap(r2) ? 1 : 0;
			}).sum();
			System.out.format("Part 2. Total of ranges that overlaps: %,d%n", total);
		}
	}
}
