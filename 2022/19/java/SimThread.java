/**
 * Simple thread that runs one simulation.
 */
public class SimThread extends Thread {
	private final Simulator simulator;
	private int max;
	private int qualityLevel;

	public SimThread(Simulator simulator) {
		super();
		this.simulator = simulator;
	}

	@Override
	public void run() {
		max = simulator.getMax();
		qualityLevel = simulator.getId() * max;
	}

	public int getMax() {
		return max;
	}

	public int getQualityLevel() {
		return qualityLevel;
	}
}
