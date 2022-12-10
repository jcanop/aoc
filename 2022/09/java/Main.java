import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Main {
	// --- Constants ---
	private static String INPUT_FILE = "../input/input.txt";

	private static int signum(int num) {
		if (num >= 1) return 1;
		if (num <= -1) return -1;
		return 0;
	}

	/**
	 * Private class that represents a Knot in a rope.
	 */
	private static class Knot {
		private int x;
		private int y;

		private Knot(int x, int y) {
			this.x = x;
			this.y = y;
		}
	}

	/**
	 * Private class that represents a rope.
	 */
	private static class Rope {
		private Knot[] list;

		private Rope(int size) {
			list = new Knot[size];
			for (int i = 0; i < list.length; i++) list[i] = new Knot(0, 0);
		}

		private Knot getTail() {
			return this.list[list.length - 1];
		}

		private void move(char direction) {
			Knot head = list[0];
			switch (direction) {
				case 'U': head.y--; break;
				case 'D': head.y++; break;
				case 'L': head.x--; break;
				case 'R': head.x++; break;
				default: throw new RuntimeException("Unsupported direction: " + direction);
			}

			for (int i = 1; i < list.length; i++) {
				Knot c = list[i - 1];
				Knot n = list[i];
				int dx = c.x - n.x;
				int dy = c.y - n.y;
				if (Math.abs(dx) < 2 && Math.abs(dy) < 2) return;
				n.x += signum(dx);
				n.y += signum(dy);
			}
		}
	}

	public static void main(String[] args) throws IOException {
		Rope rope1 = new Rope(2);
		Rope rope2 = new Rope(10);
		Knot tail1 = rope1.getTail();
		Knot tail2 = rope2.getTail();
		Set<String> set1 = new HashSet<>();
		Set<String> set2 = new HashSet<>();

		// --- Read and parse the file ---
		try (BufferedReader reader = new BufferedReader(new FileReader(INPUT_FILE))) {
			String line;
			while ((line = reader.readLine()) != null) {
				char direction = line.charAt(0);
				int count = Integer.parseInt(line.substring(2));
				for (int i = 0; i < count; i++) {
					rope1.move(direction);
					rope2.move(direction);
					String s1 = tail1.x + "," + tail1.y;
					String s2 = tail2.x + "," + tail2.y;
					set1.add(s1);
					set2.add(s2);
				}
			}
		}

		System.out.format("1. Positions visited at least once: %,d%n", set1.size());
		System.out.format("2. Positions visited at least once: %,d%n", set2.size());
	}
}
