public class Point {
	public int x;
	public int y;

	public Point(int x, int y) {
		this.x = x;
		this.y = y;
	}

	@Override
	public int hashCode() {
		return (int) (7 * Integer.hashCode(x) * Integer.hashCode(y));
	}

	@Override
	public boolean equals(Object other) {
		if (other == null) return false;
		if (this.getClass() != other.getClass()) return false;
		Point p = (Point) other;
		return this.x == p.x && this.y == p.y;
	}
}
