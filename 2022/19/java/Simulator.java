import java.util.ArrayList;
import java.util.List;

/**
 * Runs a simulation searching for the optimal pattern for a Blueprint.
 */
public class Simulator {
	private final Blueprint bp;
	private final int time;

	public Simulator(Blueprint bp, int time) {
		this.bp = bp;
		this.time = time;
	}

	public int getId() {
		return bp.getId();
	}

	public int getMax() {
		int max = 0;
		List<State> list = new ArrayList<>();
		list.add(new State(bp));

		while (list.size() > 0) {
			State state = list.remove(0);

			while (state.getTime() != time && !state.shouldBuildSomething()) state.mine();
			if (state.getTime() == time - 1) state.mine();

			if (state.getTime() == time) {
				max = Math.max(max, state.getMaterial(C.GEODE));
				continue;
			}

			if (state.canBuild(C.GEODE)) {
				state.mine();
				state.build(C.GEODE);
				list.add(state);
				continue;
			}
			for (int i = 2; i >= 0; i--) {
				if (state.canBuild(i) && state.shouldBuild(i, time)) {
					State clone = state.duplicate();
					clone.mine();
					clone.build(i);
					list.add(clone);
					state.addMask(i);
				}
			}
			state.mine();
			list.add(state);
		}

		return max;
	}
}
