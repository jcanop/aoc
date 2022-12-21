import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * Represents a Blueprint.
 */
public class Blueprint {
	private static final String REGEX = "Blueprint (\\d+): " +
		"Each ore robot costs (\\d+) ore. " +
		"Each clay robot costs (\\d+) ore. " +
		"Each obsidian robot costs (\\d+) ore and (\\d+) clay. " +
		"Each geode robot costs (\\d+) ore and (\\d+) obsidian.";
	private static final Pattern PATTERN = Pattern.compile(REGEX);

	private final int id;
	private final int[][] costs;
	private int[] maxCosts;

	public Blueprint(String line) {
		this.costs = new int[4][3];

		Matcher matcher = PATTERN.matcher(line.trim());
		matcher.find();
		this.id = Integer.parseInt(matcher.group(1));
		this.costs[C.ORE] = new int[3];
		this.costs[C.ORE][C.ORE] = Integer.parseInt(matcher.group(2));
		this.costs[C.CLAY] = new int[3];
		this.costs[C.CLAY][C.ORE] = Integer.parseInt(matcher.group(3));
		this.costs[C.OBSIDIAN] = new int[3];
		this.costs[C.OBSIDIAN][C.ORE] = Integer.parseInt(matcher.group(4));
		this.costs[C.OBSIDIAN][C.CLAY] = Integer.parseInt(matcher.group(5));
		this.costs[C.GEODE] = new int[3];
		this.costs[C.GEODE][C.ORE] = Integer.parseInt(matcher.group(6));
		this.costs[C.GEODE][C.OBSIDIAN] = Integer.parseInt(matcher.group(7));

		this.maxCosts = new int[3];
		for (int i = 0; i < 3; i++) {
			for (int j = 0; j < 4; j++) {
				this.maxCosts[i] = Math.max(maxCosts[i], costs[j][i]);
			}
		}
	}

	// --- Getters ---
	public int getId() {
		return id;
	}

	public int[] getCosts(int robot) {
		return costs[robot];
	}

	public int getMaxCosts(int robot) {
		return maxCosts[robot];
	}
}
