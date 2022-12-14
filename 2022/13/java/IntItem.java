public class IntItem extends Item implements Comparable<IntItem> {
	private final int value;

	public IntItem(String s) {
		this.value = Integer.parseInt(s);
	}

	public int unwrap() {
		return this.value;
	}

	@Override
	public int compareTo(IntItem other) {
		return this.value - other.value;
	}

	@Override
	public String toString() {
		return String.valueOf(this.value);
	}
}

