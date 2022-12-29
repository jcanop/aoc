import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Map {
	private final Blizzard blizzard;
	private final char[][] data;
	private final char[][] next;
	private final int height;
	private final int width;

	public Map(String filename) throws IOException {
		List<String> lines = new ArrayList<>();
		try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
			String line;
			while ((line = reader.readLine()) != null) {
				lines.add(line);
			}
		}
		height = lines.size();
		width = lines.get(0).length();

		blizzard = new Blizzard();
		for (int i = 1; i < lines.size() - 1; i++) {
			String line = lines.get(i);
			char[] chars = line.substring(1, line.length() - 1).toCharArray();
			blizzard.add(chars);
		}

		data = new char[height][width];
		next = new char[height][width];
	}

	private boolean canMove(int x, int y, int time) {
		if (x == 1 && y == 0) return true; // Start position
		if (x == width - 2 && y == height - 1) return true; // End position
		if (x < 1 || x >= width - 1 || y < 1 || y >= height - 1) return false; // Border
		return blizzard.isEmpty(x, y, time);
	}

	public int search(Point start, Point end, int time) {
		for (int y = 0; y < height; y++) Arrays.fill(data[y], ' ');
		data[start.y][start.x] = 'X';

		while(data[end.y][end.x] != 'X') {
			for (int y = 0; y < height; y++) Arrays.fill(next[y], ' ');
			for (int y = 0; y < height; y++) {
				for (int x = 0; x < width; x++) {
					if (data[y][x] == 'X') {
						if (canMove(x,     y, time + 1)) next[y][x]     = 'X';
						if (canMove(x - 1, y, time + 1)) next[y][x - 1] = 'X';
						if (canMove(x + 1, y, time + 1)) next[y][x + 1] = 'X';
						if (canMove(x, y - 1, time + 1)) next[y - 1][x] = 'X';
						if (canMove(x, y + 1, time + 1)) next[y + 1][x] = 'X';
					}
				}
			}
			time++;
			for (int y = 0; y < height; y++) System.arraycopy(next[y], 0, data[y], 0, width);
		}

		return time;
	}

	// --- Getters ---
	public int getWidth() {
		return width;
	}

	public int getHeight() {
		return height;
	}
}
