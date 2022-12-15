public enum Tile {
	AIR(' '), ORIGIN('+'), ROCK('#'), SAND('o');

	private final char display;

	private Tile(char display) {
		this.display = display;
	}

	public char display() {
		return this.display;
	}
}
