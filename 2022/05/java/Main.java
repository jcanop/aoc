import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {
	private static final String INPUT_FILE = "../input/input.txt";
	private static final Pattern cratesPattern = Pattern.compile("(\\[\\w\\]|\\s{3})\\s?");
	private static final Pattern stepsPattern = Pattern.compile("\\d+");

	public static void main(String[] args) throws IOException {
		List<List<Character>> queues1 = new ArrayList<>();;
		List<List<Character>> queues2 = new ArrayList<>();;

		boolean setup = true;
		try (BufferedReader reader = new BufferedReader(new FileReader(INPUT_FILE))) {
			String line;
			while ((line = reader.readLine()) != null) {
				// --- Parse Initial crates setup ---
				if (setup) {
					if (line.indexOf('[') >= 0) {
						Matcher matcher = cratesPattern.matcher(line);
						int index = 0;
						while(matcher.find()) {
							Character token = line.charAt(matcher.start() + 1);
							if (queues1.size() - 1 < index) {
								queues1.add(new ArrayList<>());
								queues2.add(new ArrayList<>());
							}
							if (token != ' ') {
								queues1.get(index).add(token);
								queues2.get(index).add(token);
							}
							index++;
						}
					}

					// --- An empty line marks ends of setup ---
					if (line.length() == 0) {
						setup = false;
					}
					continue;
				}

				// --- Parse script ---
				Matcher matcher = stepsPattern.matcher(line);
				matcher.find();
				int count = Integer.parseInt(matcher.group());
				matcher.find();
				int from = Integer.parseInt(matcher.group()) - 1;
				matcher.find();
				int to = Integer.parseInt(matcher.group()) - 1;

				// --- Puzzle 1 ---
				for (int i = 0; i < count; i++) {
					Character crate = queues1.get(from).remove(0);
					queues1.get(to).add(0, crate);
				}

				// --- Puzzle 2 ---
				for (int i = count - 1; i >= 0; i--) {
					Character crate = queues2.get(from).remove(i);
					queues2.get(to).add(0, crate);
				}
			}
		}

		System.out.print("Part 1. Crates on the top of each stack: ");
		for (List<Character> queue: queues1) System.out.print(queue.get(0));
		System.out.println();

		System.out.print("Part 2. Crates on the top of each stack: ");
		for (List<Character> queue: queues2) System.out.print(queue.get(0));
		System.out.println();
	}
}
