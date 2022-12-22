import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";
	private static final int PUZZLE1_TIME = 24;
	private static final int PUZZLE2_TIME = 32;
	private static final int PUZZLE2_BPS = 3;

	public static void main(String[] args) throws IOException, InterruptedException {
		List<Blueprint> bps = new ArrayList<>();

		// --- Reads and Parse the input file ---
		try (BufferedReader reader = new BufferedReader(new FileReader(INPUT_FILE))) {
			String line;
			while ((line = reader.readLine()) != null) {
				bps.add(new Blueprint(line));
			}
		}

		// --- Puzzle 1 ---
		SimThread[] threads = new SimThread[bps.size()];
		for (int i = 0; i < bps.size(); i++) {
			threads[i] = new SimThread(new Simulator(bps.get(i), PUZZLE1_TIME));
		}
		for (SimThread thread: threads) thread.start();
		for (SimThread thread: threads) thread.join();
		int total = 0;
		for (SimThread thread: threads) total += thread.getQualityLevel();
		System.out.format("1. Total sum of quality levels: %,d%n", total);

		// --- Puzzle 2 ---
		threads = new SimThread[PUZZLE2_BPS];
		for (int i = 0; i < threads.length; i++) {
			threads[i] = new SimThread(new Simulator(bps.get(i), PUZZLE2_TIME));
		}
		for (SimThread thread: threads) thread.start();
		for (SimThread thread: threads) thread.join();
		total = 1;
		for (SimThread thread: threads) total *= thread.getMax();
		System.out.format("2. Total of multiply the first 3 blurprints max values: %,d%n", total);
	}
}
