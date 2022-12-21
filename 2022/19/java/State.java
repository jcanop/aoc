import java.util.Arrays;

/**
 * Represents a state in the simulation
 */
public class State {
	private final Blueprint bp;
	private int[] robots;
	private int[] materials;
	private int mask;
	private int time;

	public State(Blueprint bp) {
		this.bp = bp;
		this.robots = new int[4];
		this.robots[C.ORE] = 1;
		this.materials = new int[4];
		this.mask = 0;
		this.time = 0;
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
		if (robot == C.GEODE) return true;
		int m = 1 << robot;
		if ((mask & m) == m) return false;
		int max = bp.getMaxCosts(robot);
		if (materials[robot] >= (limit - time) * max) return false;
		switch (robot) {
			case C.ORE: return robots[C.ORE] < max;
			case C.CLAY: return robots[C.CLAY] < bp.getCosts(C.OBSIDIAN)[C.CLAY];
			case C.OBSIDIAN: return robots[C.OBSIDIAN] < bp.getCosts(C.GEODE)[C.OBSIDIAN];
			default: throw new RuntimeException("Usupported robot: " + robot);
		}
	}

	public boolean shouldBuildSomething(int limit) {
		for (int i = 3; i >= 0; i--) if (canBuild(i) && shouldBuild(i, limit)) return true;
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
		clone.time = this.time;
		return clone;
	}

	@Override
	public String toString() {
		return "Robots: " + Arrays.toString(robots) +
			", materials: " + Arrays.toString(materials);
	}
}
