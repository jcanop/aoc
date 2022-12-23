public class Operation {
	private String monkey;
	private String op1;
	private String op2;
	private char operator;

	public Operation(String monkey, String s) {
		if (!s.matches("\\w+\\s+[\\-\\+\\/\\*\\=]\\s+\\w+"))
			throw new IllegalArgumentException(s);
		String[] tokens = s.split("\\s+");
		this.monkey = monkey;
		this.op1 = tokens[0];
		this.operator = tokens[1].charAt(0);
		this.op2 = tokens[2];
	}


	public void solveForVariable(int i) {
		if (i == 1) {
			switch (operator) {
				case '+': operator = '-'; break;
				case '-': operator = '+'; break;
				case '*': operator = '/'; break;
				case '/': operator = '*'; break;
				default: throw new UnsupportedOperationException("" + operator);
			}
			String tmp = op1;
			op1 = monkey;
			monkey = tmp;
		} else if (i == 2) {
			boolean swap = false;
			switch (operator) {
				case '+': operator = '-'; swap = true; break;
				case '-': operator = '-'; break;
				case '*': operator = '/'; swap = true; break;
				case '/': operator = '/'; break;
				default: throw new UnsupportedOperationException("" + operator);
			}
			String tmp = op2;
			op2 = monkey;
			monkey = tmp;
			if (swap) {
				tmp = op2;
				op2 = op1;
				op1 = tmp;
			}
		} else {
			throw new IllegalArgumentException("Invalid i: " + i);
		}
	}

	// --- Getters ---
	public String getMonkey() {
		return this.monkey;
	}

	public String getOp1() {
		return this.op1;
	}

	public String getOp2() {
		return this.op2;
	}

	public char getOperator() {
		return this.operator;
	}

	@Override
	public String toString() {
		return monkey + ": " + op1 + " " + operator + " " + op2;
	}
}
