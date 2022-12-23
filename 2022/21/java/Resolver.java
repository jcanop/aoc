import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class Resolver {
	private final List<Operation> queue;
	private final Map<String, Value> solved;

	public Resolver(List<Operation> queue, Map<String, Value> solved) {
		this.queue = queue;
		this.solved = solved;
	}

	public long solveFor(String id) {
		while (queue.size() > 0) {
			Operation op = queue.remove(0);
			String monkey = op.getMonkey();
			char operator = op.getOperator();
			Value v1 = solved.get(op.getOp1());
			Value v2 = solved.get(op.getOp2());

			if (operator == '=') {
				if (v1 != null && v2 == null) {
					addSolution(op.getOp2(), v1);
				} else if (v1 == null && v2 != null) {
					addSolution(op.getOp1(), v2);
				}
			}

			if (v1 != null && v2 != null) {
				long result = 0;
				long n1 = v1.getValue();
				long n2 = v2.getValue();
				switch (operator) {
					case '+': result = Math.addExact(n1, n2); break;
					case '-': result = Math.subtractExact(n1, n2); break;
					case '*': result = Math.multiplyExact(n1, n2); break;
					case '/': result = n1 / n2; break;
					case '=': if (n1 != n2) throw new RuntimeException(n1 + " != " +n2); break;
					default: throw new UnsupportedOperationException("" + operator);
				}
				addSolution(op.getMonkey(), new Value(result));
			} else {
				queue.add(op);
			}
		}

		return solved.get(id).getValue();
	}

	private void addSolution(String monkey, Value value) {
		solved.put(monkey, value);
		queue.stream().filter(o -> o.getMonkey().equals(monkey)).forEach(o -> {
			Value o1 = solved.get(o.getOp1());
			Value o2 = solved.get(o.getOp2());
			if (o1 != null && o2 == null) o.solveForVariable(2);
			else if (o1 == null && o2 != null) o.solveForVariable(1);
		});
	}
}
