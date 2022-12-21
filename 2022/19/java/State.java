import java.util.Arrays;

/**
 * Represents a state in the simulation
 */
public class State {
	private final Blueprint bp;
	private int[] robots;
	private int[] materials;
	private int[] maxCosts;
	private int mask;
	private int time;

	public State(Blueprint bp) {
		this.bp = bp;
		this.robots = new int[4];
		this.robots[C.ORE] = 1;
		this.materials = new int[4];
		this.maxCosts = new int[3];
		this.mask = 0;
		this.time = 0;

		for (int i = 0; i < 3; i++) {
			for (int j = 0; j < 4; j++) {
				maxCosts[i] = Math.max(maxCosts[i], bp.getCosts(j)[i]);
			}
		}
	}

	public void mine() {
		materials[C.ORE] += robots[C.ORE];
		materials[C.CLAY] += robots[C.CLAY];
		materials[C.OBSIDIAN] += robots[C.OBSIDIAN];
		materials[C.GEODE] += robots[C.GEODE];
		time++;
	}

	public boolean canBuild(int robot) {
		int[] costs = bp.getCosts(robot);
		for (int i = 0; i < 3; i++) if (materials[i] < costs[i]) return false;
		return true;
	}

	public boolean shouldBuild(int robot, int limit) {
		int m = 1 << robot;
		if ((mask & m) == m) return false;
		if (robot != C.GEODE && materials[robot] >= (limit - time) * maxCosts[robot]) return false;
		switch (robot) {
			case C.ORE:
				int max = Math.max(bp.getCosts(C.GEODE)[C.ORE], bp.getCosts(C.OBSIDIAN)[C.ORE]);
				max = Math.max(max, bp.getCosts(C.CLAY)[C.ORE]);
				max = Math.max(max, bp.getCosts(C.ORE)[C.ORE]);
				return robots[C.ORE] < max;
			case C.CLAY: return robots[C.CLAY] < bp.getCosts(C.OBSIDIAN)[C.CLAY];
			case C.OBSIDIAN: return robots[C.OBSIDIAN] < bp.getCosts(C.GEODE)[C.OBSIDIAN];
			case C.GEODE: return true;
			default: throw new RuntimeException("Usupported robot: " + robot);
		}
	}

	public boolean shouldBuildSomething() {
		for (int i = 3; i >= 0; i++) if (canBuild(i) || shouldBuild(i, time)) return true;
		return false;
	}

	public void build(int robot) {
		int[] costs = bp.getCosts(robot);
		for (int i = 0; i < 3; i++) materials[i] -= costs[i];
		robots[robot]++;
	}

	public int getMaterial(int material) {
		return materials[material];
	}

	public int getTime() {
		return time;
	}

	public void addMask(int robot) {
		mask |= (1 << robot);
	}

	protected State duplicate() {
		State clone = new State(this.bp);
		for (int i = 0; i < this.robots.length; i++) clone.robots[i] = this.robots[i];
		for (int i = 0; i < this.materials.length; i++) clone.materials[i] = this.materials[i];
		clone.maxCosts = this.maxCosts;
		clone.time = this.time;
		return clone;
	}

	@Override
	public String toString() {
		return "Robots: " + Arrays.toString(robots) +
			", materials: " + Arrays.toString(materials);
	}
}
