public class SNAFU {
	public static String to(long value) {
		StringBuilder sb = new StringBuilder();
		long  overflow = 0;
		while (value > 0) {
			int rem = (int) (value % 5);
			switch (rem) {
				case 0: sb.insert(0, '0'); break;
				case 1: sb.insert(0, '1'); break;
				case 2: sb.insert(0, '2'); break;
				case 3: sb.insert(0, '='); overflow++; break;
				case 4: sb.insert(0, '-'); overflow++; break;
				default: throw new RuntimeException("Unsupported case: " + rem);
			}
			value /= 5;
			value += overflow;
			overflow = 0;
		}
		return sb.toString();
	}

	public static long from(String value) {
		long result = 0;
		char[] chars = value.toCharArray();
		for (int i = chars.length - 1; i >= 0; i--) {
			long pos = (long) Math.pow(5D, (double) (chars.length - i - 1));
			switch(chars[i]) {
				case '0': break;
				case '1': result += (1 * pos); break;
				case '2': result += (2 * pos); break;
				case '=': result -= (2 * pos); break;
				case '-': result -= (1 * pos); break;
				default: throw new RuntimeException("Unsupported case: " + chars[i]);
			}
		}
		return result;
	}

	public static void main(String[] args) {
		// --- Data ---
		long[] decimals = new long[] { 1, 2, 3, 4, 5, 6,  7, 8, 9, 10, 15, 20, 2022,
			12345, 314159265, 1747, 906, 198, 11, 201, 31, 1257, 32, 353, 107, 7, 3,
			37, 4890 };
		String[] snafus = new String[] { "1", "2", "1=", "1-", "10", "11", "12", "2=",
			"2-", "20", "1=0","1-0", "1=11-2", "1-0---0", "1121-1110-1=0", "1=-0-2",
			"12111", "2=0=", "21", "2=01", "111", "20012", "112", "1=-1=", "1-12", "12",
			"1=", "122", "2=-1=0" };

		// --- Test 1 ---
		System.out.println("--- Testing: Decimal => SNAFU ---");
		for (int i = 0; i < decimals.length; i++) {
			String result = to(decimals[i]);
			System.out.format("[%s] %d => %s%n",
				(snafus[i].equals(result) ? "\u001B[32mOK\u001B[0m": "\u001B[31mERROR\u001B[0m"),
				decimals[i], result);
		}

		// --- Test 2 ---
		System.out.println("\n--- Testing: SNAFU => Decimal ---");
		for (int i = 0; i < decimals.length; i++) {
			long  result = from(snafus[i]);
			System.out.format("[%s] %s => %d%n",
				(decimals[i] == result ? "\u001B[32mOK\u001B[0m": "\u001B[31mERROR\u001B[0m"),
				snafus[i], result);
		}
	}
}
