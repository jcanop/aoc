import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

/**
 * Represents a Monkey in the game.
 */
public class Monkey {
	private final int id;
	private final List<Long> items;
	private final char operator;
	private final Integer operand;
	private final int divisible;
	private final int throwTrue;
	private final int throwFalse;
	private final int relief;
	private final List<Monkey> group;
	private int inspects;
	private long gcd;

	/**
	 * Creates a new Monkey.
	 *
	 * @param id Monkey's ID.
	 * @param items Initial items owned by the moneky.
	 * @param operator Inspect's operator.
	 * @param operand Inspect's operand.
	 * @param divisible Test's operand.
	 * @param throwTrue Monkey to throw the item if test result is true.
	 * @param throwFalse Monkey to throw the item if test result is false.
	 * @param relief Type of relief per inspection. 1 or 2 depending the part of the puzzle.
	 * @param group The group of monkeys playing.
	 */
	public Monkey(int id, long[] items, char operator, Integer operand, int divisible, int throwTrue, int throwFalse, int relief, List<Monkey> group) {
		this.id = id;
		this.items = LongStream.of(items).boxed().collect(Collectors.toCollection(ArrayList::new));
		this.operator = operator;
		this.operand = operand;
		this.divisible = divisible;
		this.throwTrue = throwTrue;
		this.throwFalse = throwFalse;
		this.relief = relief;
		this.group = group;
		this.inspects = 0;
		this.gcd = 0;
	}

	private void receive(long item) {
		items.add(item);
	}

	public void play() {
		while (items.size() > 0) {
			inspects++;
			long item = items.remove(0);
			long value = operand == null ? item : operand;
			switch (operator) {
				case '+': item += value; break;
				case '*': item *= value; break;
				default: throw new RuntimeException("Usupported operator: " + operator);
			}
			if (relief == 1) {
				item /= 3;
			} else if (relief == 2) {
				if (gcd == 0) gcd = group.stream()
					.mapToLong(m -> m.divisible)
					.reduce(1L, (a, b) -> a * b);
				item %= gcd;
			} else {
				throw new RuntimeException("Usupported relief: " + relief);
			}
			Monkey m = group.get(item % divisible == 0 ? throwTrue : throwFalse);
			m.receive(item);
		}
	}

	@Override
	public String toString() {
		return "[Monkey] " + (items.size() > 0 ? items : "No items") + ", inspects: " + inspects;
	}

	public int getInspects() {
		return inspects;
	}
}
