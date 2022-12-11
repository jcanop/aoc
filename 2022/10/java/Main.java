import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";

	/**
	 * Private class that represents a Command.
	 */
	private static class Command {
		private int cycles;
		private Command(int cycles) { this.cycles = cycles; }
	}

	/**
	 * Private class that represents a NoOp Command.
	 */
	private static class NoOp extends Command {
		private NoOp() { super(1); }
	}

	/**
	 * Private class that represents a AddX Command.
	 */
	private static class AddX extends Command {
		private int value;
		private AddX(int value) { super(2); this.value = value; }
	}

	/**
	 * Private class that represents the CPU.
	 */
	private static class CPU {
		private List<Command> program;
		private int register;

		private CPU(List<Command> program) {
			this.program = program;
			this.register = 1;
		}

		/**
		 * Process a cycle.
		 *
		 * @return true if there is commands to execute, otherwise false.
		 */
		private boolean tick() {
			if (program.size() == 0) return false;
			Command cmd = program.get(0);
			cmd.cycles--;
			if (cmd.cycles > 0) return true;
			if (cmd instanceof AddX) register += ((AddX) cmd).value;
			else if (!(cmd instanceof NoOp)) throw new RuntimeException("Unsupported commmand: " + cmd);
			program.remove(0);
			return program.size() > 0;
		}
	}

	public static void main(String[] args) throws IOException {
		List<Command> program;

		// --- Reads and parse the input file ---
		try (Stream<String> stream = Files.lines(Paths.get(INPUT_FILE))) {
			program = stream.map(line -> {
				if (line.equals("noop")) return new NoOp();
				if (line.startsWith("addx ")) return new AddX(Integer.parseInt(line.substring(5)));
				throw new IllegalArgumentException("Unknowun commmand: " + line);
			}).collect(Collectors.toList());
		}

		// --- Execute the program in the CPU ---
		CPU cpu = new CPU(program);
		int cycle = 1;
		int mark = 20;
		int total = 0;
		StringBuilder sb = new StringBuilder('\n');
		do {
			// --- Puzzle 1 ---
			if (cycle == mark) {
				total += cpu.register * mark;
				mark += 40;
			}

			// --- Puzzle 2 ---
			int p = (cycle - 1) % 40;
			char c = (p >= cpu.register - 1 && p <= cpu.register + 1) ? '#' : ' ';
			sb.append(c);
			if (cycle % 40 == 0) sb.append('\n');

			// --- Both ---
			cycle++;
		} while (cpu.tick());

		// --- Puzzle 1 ---
		System.out.format("1. The sum of the signal strenghts is: %,d%n", total);

		// --- Puzzle 2 ---
		System.out.println();
		System.out.println("2. Image on the CRT");
		System.out.println(sb.toString());
	}
}
