public class Value {
	private final long value;

	public Value(String s) {
		this.value = Long.parseLong(s);
	}

	public Value(long v) {
		this.value = v;
	}

	public long getValue() {
		return this.value;
	}

	@Override
	public String toString() {
		return String.valueOf(this.value);
	}
}
