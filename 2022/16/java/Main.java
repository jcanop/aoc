import java.io.IOException;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";

	public static void main(String[] args) throws IOException {
		Layout layout = Layout.load(INPUT_FILE);
		long total = layout.dfs1();
		System.out.format("1. Total pressure released: %,d%n", total);
		total = layout.dfs2();
		System.out.format("2. Total pressure released with an elephant: %,d%n", total);
	}
}
