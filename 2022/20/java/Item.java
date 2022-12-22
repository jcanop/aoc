/**
 * Simple wrapper class for the numbers in the list, this classes is needed
 * because numbers can repeat on the input file.
 */
public class Item {
	private static int INDEX = 0;
	private final int id;
	private long value;

	public Item(String value) {
		this.id = INDEX++;
		this.value = Long.parseLong(value);
	}

	public long getValue() {
		return value;
	}

	public void addKey(long key) {
		this.value = Math.multiplyExact(value, key);
	}

	@Override
	public int hashCode() {
		return id;
	}

	@Override
	public boolean equals(Object o) {
		if (o == null) return false;
		if (this.getClass() != o.getClass()) return false;
		Item other = (Item) o;
		return id == other.id;
	}

	@Override
	public String toString() {
		return String.valueOf(value);
	}
}
