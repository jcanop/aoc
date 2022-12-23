import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Main {
	private static final String INPUT_FILE = "../input/input.txt";

	public static void main(String[] args) throws IOException {
		String data = new String(Files.readAllBytes(Paths.get(INPUT_FILE)));
		String[] parts = data.split("\\n\\n");

		// --- Puzzle 1 ---
		Map map = new Map(parts[0]);
		map.path(parts[1]);
		long password = map.getPassword();
		System.out.format("1. Password %,d%n", password);
	}
}
