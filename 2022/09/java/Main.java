import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class Main {
	private static String INPUT_FILE = "../input/input.txt";

	private static class Knot {
		private int x;
		private int y;

		private Knot(int x, int y) {
			this.x = x;
			this.y = y;
		}

		private Knot copy() {
			return new Knot(this.x, this.y);
		}

		private Set<String> calculateTrail(Knot k, char direction) {
			Set<String> set = new HashSet<String>();
			set.add(k.x + "," + k.y);
			if (Math.abs(this.x - k.x) < 2 && Math.abs(this.y - k.y) < 2) return set;
			switch (direction) {
				case 'U': for (int i = k.y - 1; i >= this.y; i--) set.add(this.x + "," + i); break;
				case 'D': for (int i = k.y + 1; i <= this.y; i++) set.add(this.x + "," + i); break;
				case 'L': for (int i = k.x - 1; i >= this.x; i--) set.add(i + "," + this.y); break;
				case 'R': for (int i = k.x + 1; i <= this.x; i++) set.add(i + "," + this.y); break;
				default: throw new RuntimeException("Unsupported direction: " + direction);
			}
			return set;
		}
	}

	private static class Rope {
		private Knot head;
		private Knot tail;

		private Rope(Knot head, Knot tail) {
			this.head = head;
			this.tail = tail;
		}

		private void move(char direction, int count) {
			switch (direction) {
				case 'U': head.y -= count; break;
				case 'D': head.y += count; break;
				case 'L': head.x -= count; break;
				case 'R': head.x += count; break;
				default: throw new RuntimeException("Unsupported direction: " + direction);
			}
			if (Math.abs(head.x - tail.x) < 2 && Math.abs(head.y - tail.y) < 2) return;
			switch (direction) {
				case 'U': tail.y = head.y + 1; tail.x = head.x; break;
				case 'D': tail.y = head.y - 1; tail.x = head.x; break;
				case 'L': tail.x = head.x + 1; tail.y = head.y; break;
				case 'R': tail.x = head.x - 1; tail.y = head.y; break;
				default: throw new RuntimeException("Unsupported direction: " + direction);
			}
		}
	}

	public static void main(String[] args) throws IOException {
		Knot head = new Knot(0, 4);
		Knot tail = new Knot(0, 4);
		Rope rope = new Rope(head, tail);
		Set<String> set = new HashSet<>();

		try (BufferedReader reader = new BufferedReader(new FileReader(INPUT_FILE))) {
			String line;
			while ((line = reader.readLine()) != null) {
				char direction = line.charAt(0);
				int count = Integer.parseInt(line.substring(2));

				Knot old = tail.copy();
				rope.move(direction, count);
				set.addAll(tail.calculateTrail(old, direction));
				//System.out.println(line);
				//System.out.println(tail.calculateTrail(old, direction));
			}
		}
		System.out.println("1. " + set.size());
	}
}
