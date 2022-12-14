import java.util.List;
import java.util.stream.Collectors;

/**
 * Divide the string into tokens
 */
public class Tokenizer {
	private List<Character> list;

	public Tokenizer(String s) {
		s = s.replaceAll("(^\\[|\\]$)", "");
		this.list = s.chars().mapToObj(e -> (char) e).collect(Collectors.toList());
	}

	/**
	 * Get the next token.
	 *
	 * @return A String of the next token or null if not other token is available.
	 */
	public String next() {
		int count = 0;
		StringBuilder sb = new StringBuilder();
		while (list.size() > 0) {
			char c = list.remove(0);
			if (c == '[') {
				sb.append(c);
				count++;
			} else if (c == ',') {
				if (count == 0) return sb.toString();
				sb.append(c);
			} else if (c == ']') {
				sb.append(c);
				if (count == 0) return sb.toString();
				count--;
			} else {
				sb.append(c);
			}
		}
		if (sb.length() == 0) return null;
		return sb.toString();
	}
}
