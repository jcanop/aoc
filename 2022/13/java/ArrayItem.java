import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;

public class ArrayItem extends Item implements Comparable<ArrayItem> {
	private final Item[] value;

	public ArrayItem(String s) {
		List<Item> list = new ArrayList<>();
		Tokenizer tokenizer = new Tokenizer(s);
		String token;
		while ((token = tokenizer.next()) != null) {
			if (token.startsWith("[")) list.add(new ArrayItem(token));
			else list.add(new IntItem(token));
		}
		this.value = list.toArray(new Item[list.size()]);
	}

	@Override
	public int compareTo(ArrayItem other) {
		int len = Math.max(this.value.length, other.value.length);
		for (int i = 0; i < len; i++) {
			if (i == this.value.length) return -1;
			if (i == other.value.length) return 1;
			Item a = this.value[i];
			Item b = other.value[i];
			if (a instanceof IntItem && b instanceof IntItem) {
				int r = ((IntItem) a).compareTo(((IntItem) b));
				if (r != 0) { return r; }
			} else if (a instanceof ArrayItem && b instanceof ArrayItem) {
				int r = ((ArrayItem) a).compareTo(((ArrayItem) b));
				if (r != 0) { return r; }
			} else if (a instanceof IntItem && b instanceof ArrayItem) {
				ArrayItem ai = new ArrayItem("[" + ((IntItem) a).unwrap() + "]");
				int r = ai.compareTo((ArrayItem) b);
				if (r != 0) { return r; }
			} else if (a instanceof ArrayItem && b instanceof IntItem) {
				ArrayItem bi = new ArrayItem("[" + ((IntItem) b).unwrap() + "]");
				int r = ((ArrayItem) a).compareTo(bi);
				if (r != 0) { return r; }
			} else {
				throw new RuntimeException("Invalid combination");
			}
		}
		return 0;
	}

	@Override
	public String toString() {
		return String.valueOf(Arrays.asList(this.value)).replaceAll("\\s+", "");
	}
}
