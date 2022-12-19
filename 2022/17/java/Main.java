import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";
	private static final long LIMIT_1 = 2022;
	private static final long LIMIT_2 = 1_000_000_000_000L;

	// --- Prints the chmaber on the console for debuging ---
	public static void print(Chamber chamber) throws IOException {
		System.out.print("\033[H\033[2J");
		chamber.print();
		System.in.read();
	}

	// --- Runs the simuations ---
	public static long simulation(char[] flows, long limit) {
		ShapeFactory factory = new ShapeFactory();
		Chamber chamber = new Chamber();
		chamber.addShape(factory.next());

		// --- Variables for tracking a pattern ---
		long prevH = 0;
		long prevR = 0;
		long deltaH = 0;
		long deltaR = 0;
		long simulated = 0;

		// --- Iterate until the limit is reached ---
		long count = 0;
		long rocks = 1;
		while (true) {
			// --- Tries to find a pattern ---
			if (count > 0 && count % (flows.length * factory.getCount()) == 0) {
				long height = chamber.getHeight();
				long dh = (height - prevH);
				long dr = (rocks - prevR);
				// --- Patern found ----
				if (dh == deltaH && dr == deltaR) {
					long rate = (limit - rocks) / dr;
					simulated = dh * rate + 1;
					rocks = limit - ((limit - rocks) % dr);
				}
				prevH = height;
				prevR = rocks;
				deltaH = dh;
				deltaR = dr;
			}

			// --- Normal simulation ---
			int index = (int) (count % flows.length);
			char flow = flows[index];
			count++;
			chamber.move(flow);
			if (!chamber.down()) {
				if (rocks > limit) {
					return chamber.getHeight() + simulated;
				}
				chamber.addShape(factory.next());
				rocks++;
			}
		}
	}

	public static void main(String[] args) throws IOException {
		// --- Read the input file ---
		char[] flows = Files.readString(Paths.get(INPUT_FILE)).trim().toCharArray();

		// --- Puzzle 1 ---
		long height = simulation(flows, LIMIT_1);
		System.out.format("1. The tower is %,d units tall%n", height);

		// --- Puzzle 2 ---
		height = simulation(flows, LIMIT_2);
		System.out.format("2. The tower is %,d units tall%n", height);
	}
}
