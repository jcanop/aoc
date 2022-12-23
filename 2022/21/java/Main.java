import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";
	private static final String ROOT = "root";
	private static final String ME = "humn";

	public static void main(String[] args) throws IOException {
		// --- Puzzle 1 ---
		final List<Operation> queue1 = new ArrayList<>();
		final Map<String, Value> solved1 = new HashMap<>();
		try (Stream<String> stream = Files.lines(Paths.get(INPUT_FILE))) {
			stream.forEach(line -> {
				String[] tokens = line.split("\\s*:\\s*");
				String key = tokens[0];
				String value = tokens[1];
				if (value.matches("\\-?\\d+")) {
					solved1.put(key, new Value(value));
				} else {
					queue1.add(new Operation(key, value));
				}
			});
		}
		Resolver resolver = new Resolver(queue1, solved1);
		long result = resolver.solveFor(ROOT);
		System.out.format("1. Number that will the monkey root yell: %,d%n", result);

		// --- Puzzle 2 ---
		final List<Operation> queue2 = new ArrayList<>();
		final Map<String, Value> solved2 = new HashMap<>();
		try (Stream<String> stream = Files.lines(Paths.get(INPUT_FILE))) {
			stream.forEach(line -> {
				String[] tokens = line.split("\\s*:\\s*");
				String key = tokens[0];
				String value = tokens[1];
				if (value.matches("\\-?\\d+")) {
					if (!key.equals(ME)) solved2.put(key, new Value(value));
				} else {
					Operation op = new Operation(key, value);
					String op1 = op.getOp1();
					String op2 = op.getOp2();
					char operator = op.getOperator();
					if (key.equals(ROOT)) op = new Operation(ROOT, op1 + " = " + op2);
					if (op1.equals(ME)) {
						op.solveForVariable(1);
					} else if (op2.equals(ME)) {
						op.solveForVariable(2);
					}
					queue2.add(op);
				}
			});
		}
		resolver = new Resolver(queue2, solved2);
		result = resolver.solveFor(ME);
		System.out.format("2. Number that I yell to pass root's equality test: %,d%n", result);
	}
}
