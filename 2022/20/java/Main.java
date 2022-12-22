import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";
	private static final long KEY = 811589153;
	private static final int MIX_COUNT = 10;
	private static final int OFFSET_X = 1_000;
	private static final int OFFSET_Y = 2_000;
	private static final int OFFSET_Z = 3_000;

	// --- Mix the order of numbers ---
	private static void mix(List<Item> list, List<Item> order) {
		long len = order.size() - 1;
		for (Item item: order) {
			long n = item.getValue();
			if (n == 0) continue;
			long i = list.indexOf(item);
			list.remove((int) i);
			i = (i + n) % len;
			if (i < 0) i = len + i;
			if (i == 0) i = len;
			list.add((int) i, item);
		}
	}

	// --- Sum the 3 coordiantes ---
	private static long sum(List<Item> list) {
		Item zero = list.stream().filter(i -> i.getValue() == 0).findAny().get();
		int i = list.indexOf(zero);
		int len = list.size();
		long x = list.get((i + OFFSET_X) % len).getValue();
		long y = list.get((i + OFFSET_Y) % len).getValue();
		long z = list.get((i + OFFSET_Z) % len).getValue();
		return Math.addExact(Math.addExact(x, y), z);
	}

	// --- Main method ---
	public static void main(String[] args) throws IOException {
		List<Item> order;

		// --- Read and parse the input file ---
		try (Stream<String> stream = Files.lines(Paths.get(INPUT_FILE))) {
			order = stream.map(line -> new Item(line)).collect(Collectors.toList());
		}

		// --- Puzzle 1 ---
		List<Item> list = new ArrayList<>(order);
		mix(list, order);
		long total = sum(list);
		System.out.format("1. The sum of the coordinates is: %,d%n", total);

		// --- Puzzle 2 ---
		order.forEach(item -> item.addKey(KEY));
		list = new ArrayList<>(order);
		for (int i = 0; i < MIX_COUNT; i++) {
			mix(list, order);
		}
		total = sum(list);
		System.out.format("2. The sum of the coordinates is: %,d%n", total);
	}
}
