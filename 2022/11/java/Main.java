import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";
	private static final String PARSER_REGEX = "Monkey (\\d+):\\s+" +
		"Starting items: ([\\d, ]+)\\s+" +
		"Operation: new = old ([\\*\\+]) (\\d+|old)\\s+" +
		"Test: divisible by (\\d+)\\s+" +
		"If true: throw to monkey (\\d+)\\s+" +
		"If false: throw to monkey (\\d+)";

	public static void main(String[] args) throws IOException {
		List<Monkey> group1 = new ArrayList<>();
		List<Monkey> group2 = new ArrayList<>();

		// --- Read and parse the input file ---
		String text = Files.readString(Paths.get(INPUT_FILE));
		Pattern pattern = Pattern.compile(PARSER_REGEX);
		Matcher matcher = pattern.matcher(text);
		while (matcher.find()) {
			int id = Integer.parseInt(matcher.group(1));
			long[] items = Arrays.stream(matcher.group(2).split("\\s*,\\s*"))
				.mapToLong(Long::parseLong).toArray();
			char operator = matcher.group(3).charAt(0);
			String s = matcher.group(4);
			Integer operand = s.equals("old") ? null : Integer.valueOf(s);
			int divisible = Integer.parseInt(matcher.group(5));
			int throwTrue = Integer.parseInt(matcher.group(6));
			int throwFalse = Integer.parseInt(matcher.group(7));

			// --- Puzzle 1 ---
			Monkey monkey1 = new Monkey(id, items, operator, operand, divisible, throwTrue, throwFalse, 1, group1);
			group1.add(monkey1);

			// --- Puzzle 2 ---
			Monkey monkey2 = new Monkey(id, items, operator, operand, divisible, throwTrue, throwFalse, 2, group2);
			group2.add(monkey2);
		}

		// --- Puzzle 1 ---
		for (int i = 0; i < 20; i++) for (Monkey m: group1) m.play();
		long mb = group1.stream()
			.mapToLong(m -> m.getInspects())
			.boxed()
			.sorted(Comparator.reverseOrder())
			.limit(2)
			.reduce(1L, (a, b) -> a * b);
		System.out.format("1. Level of monkey business: %,d%n", mb);

		// --- Puzzle 2 ---
		for (int i = 0; i < 10_000; i++) for (Monkey m: group2) m.play();
		mb = group2.stream()
			.mapToLong(m -> m.getInspects())
			.boxed()
			.sorted(Comparator.reverseOrder())
			.limit(2)
			.reduce(1L, (a, b) -> a * b);
		System.out.format("2. Level of monkey business: %,d%n", mb);
	}
}
