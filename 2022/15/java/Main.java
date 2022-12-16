import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";
	private static final int PUZZLE1_LINE = 2_000_000;
	private static final int PUZZLE2_MIN = 0;
	private static final int PUZZLE2_MAX = 4_000_000;
	private static final String REGEX = "Sensor at x=(-?\\d+), y=(-?\\d+): " +
		"closest beacon is at x=(-?\\d+), y=(-?\\d+)";

	// --- Helper functions ---
	public static boolean inLimit(int x, int y) {
		return  x >= PUZZLE2_MIN && x <= PUZZLE2_MAX &&
				y >= PUZZLE2_MIN && y <= PUZZLE2_MAX;
	}

	public static long calculateFreq(int x, int y) {
		long lx = (long) x;
		long ly = (long) y;
		long lm = (long) PUZZLE2_MAX;
		return lx * lm + ly;
	}

	// --- Main function ---
	public static void main(String[] args) throws IOException {
		Map map = new Map();

		// --- Read and Parse the input file ---
		String text = Files.readString(Paths.get(INPUT_FILE));
		Pattern pattern = Pattern.compile(REGEX);
		Matcher matcher = pattern.matcher(text);
		while (matcher.find()) {
			int sx = Integer.parseInt(matcher.group(1));
			int sy = Integer.parseInt(matcher.group(2));
			int bx = Integer.parseInt(matcher.group(3));
			int by = Integer.parseInt(matcher.group(4));
			map.addSensor(sx, sy, bx, by);
		}

		// --- Search Max and Min ---
		int minx = Integer.MAX_VALUE;
		int maxx = 0;
		int range = 0;
		for (Sensor s: map.getSensors()) {
			minx = Math.min(minx, s.getX());
			maxx = Math.max(maxx, s.getX());
			range = Math.max(range, s.getRange());
		}
		minx -= range;
		maxx += range;

		// --- Puzzle 1 ---
		long total = 0;
		for (int x = minx; x <= maxx; x++) {
			if (map.isInSensorRange(x, PUZZLE1_LINE) && map.isEmpty(x, PUZZLE1_LINE)) {
				total++;
			}
		}
		System.out.format("1. Positions that cannot contain a beacon: %,d%n", total);

		// --- Puzzle 2 ---
		long freq = 0;
		for (Sensor sensor: map.getSensors()) {
			int sx = sensor.getX();
			int sy = sensor.getY();
			int sr = sensor.getRange();
			for (int i = 0; i <= sr; i++) {
				// North -> East
				int x = sx + i;
				int y = sy - sr + i - 1;
				if (inLimit(x, y) && !map.isInSensorRange(x, y)) {
					freq = calculateFreq(x, y); break;
				}

				// East -> South
				x = sx + sr - i + 1;
				y = sy + i;
				if (inLimit(x, y) && !map.isInSensorRange(x, y)) {
					freq = calculateFreq(x, y); break;
				}

				// South -> West
				x = sx - i;
				y = sy + sr - i + 1;
				if (inLimit(x, y) && !map.isInSensorRange(x, y)) {
					freq = calculateFreq(x, y); break;
				}

				// West -> North
				x = sx - sr + i - 1;
				y = sy - i;
				if (inLimit(x, y) && !map.isInSensorRange(x, y)) {
					freq = calculateFreq(x, y); break;
				}
			}
			if (freq > 0) break;
		}
		System.out.format("2. Fequency: %,d%n", freq);
	}
}
