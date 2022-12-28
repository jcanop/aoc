public class Elf {
	private Point position;

	public Elf(int x, int y) {
		this.position = new Point(x, y);
	}

	// --- Getters & Setters ---
	public Point getPosition() {
		return position;
	}

	public void setPosition(Point p) {
		this.position = p;
	}
}
