public class Point {
	public final int x;
	public final int y;
	public final int z;

	public Point(int x, int y, int z) {
		this.x = x;
		this.y = y;
		this.z = z;
	}

	public Point from(int x, int y, int z) {
		return new Point(this.x + x, this.y + y, this.z + z);
	}

	@Override
	public int hashCode() {
		int hash = 7;
		hash = 31 * hash + x;
		hash = 31 * hash + y;
		hash = 31 * hash + z;
		return hash;
	}

	@Override
	public boolean equals(Object o) {
		if (o == this) return true;
		if (!(o instanceof Point)) return false;
		Point p = (Point) o;
		return this.x == p.x && this.y == p.y && this.z == p.z;
	}

	@Override
	public String toString() {
		return x + "," + y + "," + z;
	}
}
