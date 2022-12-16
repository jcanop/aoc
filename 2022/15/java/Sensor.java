public class Sensor {
	private final int x;
	private final int y;
	private final int range;

	public Sensor(int x, int y, int bx, int by) {
		this.x = x;
		this.y = y;
		this.range = Math.abs(x - bx) + Math.abs(y -by);
	}

	public boolean isInRange(int x, int y) {
		return Math.abs(this.x - x) + Math.abs(this.y - y) <= this.range;
	}

	public int getX() {
		return this.x;
	}

	public int getY() {
		return this.y;
	}

	public int getRange() {
		return this.range;
	}

	@Override
	public String toString() {
		return "[Sensor] (" + x + ", " + y + ") range: " + range;
	}
}
