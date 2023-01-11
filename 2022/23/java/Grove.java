import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class Grove {
	private static final int OFFSET = 10_000;

	private final Map<Point, Elf> elves;
	private final Map<Point, Elf> proposed;
	private final Set<Point> banned;
	private int round;
	private int minx;
	private int miny;
	private int maxx;
	private int maxy;

	/**
	 * Constructor
	 *
	 * @param filename Input filename
	 * @throws IOException When an I/O error ocurrs
	 */
	public Grove(String filename) throws IOException {
		elves = new HashMap<>();
		proposed = new HashMap<>();
		banned = new HashSet<>();
		round = 0;

		try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
			int y = 0;
			String line;
			while ((line = reader.readLine()) != null) {
				char[] chars = line.toCharArray();
				for (int x = 0; x < chars.length; x++) {
					if (chars[x] == '#') {
						Elf elf = new Elf(OFFSET + x, OFFSET + y);
						elves.put(elf.getPosition(), elf);
					}
				}
				y++;
			}
		}
	}

	/**
	 * Checks for Elves collisions
	 *
	 * @param elf Current elf to check
	 * @param p Proposed position to move
	 */
	private void check(Elf elf, Point p) {
		Point pos = new Point(p.x, p.y);
		if (banned.contains(pos)) {
			proposed.put(elf.getPosition(), elf);
		} else if (proposed.containsKey(pos)) {
			banned.add(pos);
			Elf e = proposed.remove(pos);
			proposed.put(e.getPosition(), e);
			proposed.put(elf.getPosition(), elf);
		} else {
			proposed.put(pos, elf);
		}
	}

	/**
	 * Simulate a round
	 *
	 * @return True if there was at least one move, otherwise false
	 */
	public boolean sim() {
		Point p = new Point(0, 0);
		boolean result = false;

		// --- Phase 1: Considers move  ---
		for (Elf elf: elves.values()) {
			Point pos = elf.getPosition();

			// --- Look at the 8 positions adjacent ---
			boolean found = false;
			for (int y = -1; y <= 1; y++) {
				for (int x = -1; x <= 1; x++) {
					if (x == 0 && y == 0) continue;
					p.x = pos.x + x; p.y = pos.y + y;
					if (elves.containsKey(p)) {
						found = true;
						break;
					}
				}
				if (found) break;
			}
			if (!found) {
				// --- No Elf adjacent, then do nothing ---
				proposed.put(elf.getPosition(), elf);
				continue;
			}

			// --- Look at the 4 directions ---
			boolean added = false;
			for (int n = 0; n < 4; n++) {
				int i = (n + round) % 4;
				if (i == 0) {
					p.x = pos.x - 1; p.y = pos.y - 1; // NW
					if (elves.containsKey(p)) continue;
					p.x = pos.x + 1; p.y = pos.y - 1; // NE
					if (elves.containsKey(p)) continue;
					p.x = pos.x + 0; p.y = pos.y - 1; // N
					if (elves.containsKey(p)) continue;
					check(elf, p);
					added = true;
					break;
				} else if (i == 1) {
					p.x = pos.x - 1; p.y = pos.y + 1; // SW
					if (elves.containsKey(p)) continue;
					p.x = pos.x + 1; p.y = pos.y + 1; // SE
					if (elves.containsKey(p)) continue;
					p.x = pos.x + 0; p.y = pos.y + 1; // S
					if (elves.containsKey(p)) continue;
					check(elf, p);
					added = true;
					break;
				} else if (i == 2) {
					p.x = pos.x - 1; p.y = pos.y - 1; // NW
					if (elves.containsKey(p)) continue;
					p.x = pos.x - 1; p.y = pos.y + 1; // SW
					if (elves.containsKey(p)) continue;
					p.x = pos.x - 1; p.y = pos.y + 0; // W
					if (elves.containsKey(p)) continue;
					check(elf, p);
					added = true;
					break;
				} else if (i == 3) {
					p.x = pos.x + 1; p.y = pos.y - 1; // NE
					if (elves.containsKey(p)) continue;
					p.x = pos.x + 1; p.y = pos.y + 1; // SE
					if (elves.containsKey(p)) continue;
					p.x = pos.x + 1; p.y = pos.y + 0; // E
					if (elves.containsKey(p)) continue;
					check(elf, p);
					added = true;
					break;
				} else {
					throw new RuntimeException("Unreachable!");
				}
			}
			result |= added;
			if (!added) proposed.put(elf.getPosition(), elf);
		}

		// --- Phase 2: Move ---
		for (Point pos: proposed.keySet()) {
			proposed.get(pos).setPosition(pos);
		}
		elves.clear();
		elves.putAll(proposed);
		proposed.clear();
		banned.clear();
		round++;

		return result;
	}

	/**
	 * Private method that find the View Rectangle
	 */
	private void findViewRect() {
		minx = Integer.MAX_VALUE;
		miny = Integer.MAX_VALUE;
		maxx = 0;
		maxy = 0;
		for (Point p: elves.keySet()) {
			minx = Math.min(minx, p.x);
			miny = Math.min(miny, p.y);
			maxx = Math.max(maxx, p.x);
			maxy = Math.max(maxy, p.y);
		}
	}

	/**
	 * Count the empty tiles on the View Rectangle
	 *
	 * @return the number of empty tiles
	 */
	public int emptyCount() {
		int total = 0;
		findViewRect();
		Point p = new Point(0, 0);
		for (int y = miny; y <= maxy; y++) {
			for (int x = minx; x <= maxx; x++) {
				p.x = x; p.y = y;
				if (!elves.containsKey(p)) total++;
			}
		}
		return total;
	}

	/**
	 * Prints the grove into the console
	 */
	public void print() {
		findViewRect();
		Point p = new Point(0, 0);
		for (int y = miny; y <= maxy; y++) {
			for (int x = minx; x <= maxx; x++) {
				p.x = x; p.y = y;
				System.out.print(elves.containsKey(p) ? '#' : '.');
			}
			System.out.println();
		}
	}
}
