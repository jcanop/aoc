import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class Main {
	private static final String INPUT_FILE = "../input/input.txt";

	public static void main(String[] args) throws IOException {
		try (Stream<String> stream = Files.lines(Paths.get(INPUT_FILE))) {
			long total = stream.mapToLong(line -> SNAFU.from(line)).sum();
			String snafu = SNAFU.to(total);
			System.out.format("SNAFU number to supply to Bob's console: %s%n", snafu);
		}
	}
}
