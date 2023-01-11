import Operation from "./operation.mjs";

function Resolver(text) {
	this.queue = [];
	this.solved = {};

	for (const line of text.trim().split('\n')) {
		if (line.match(/\w+:\s+\d+/)) {
			const i = line.indexOf(":");
			const key = line.substring(0, i);
			const value = Number(line.substring(i + 2));
			this.solved[key] = value;
		} else {
			this.queue.push(new Operation(line));
		}
	}

	this.updateForPuzzle2 = function(root, me) {
		for (let op of this.queue) {
			if (op.key === root) op.operator = "=";
			if (op.op1 === me) op.solveForVariable(1);
			else if (op.op2 === me) op.solveForVariable(2);
		}
		delete this.solved[me];
	};

	this.solveFor = function(id) {
		while (this.queue.length > 0) {
			let op = this.queue.shift();
			const v1 = this.solved[op.op1];
			const v2 = this.solved[op.op2];

			if (op.operator === '=') {
				if (v1 !== undefined && v2 === undefined) {
					this.addSolution(op.op2, v1);
				} else if (v1 === undefined && v2 !== undefined) {
					this.addSolution(op.op1, v2);
				}
			}

			if (v1 !== undefined && v2 !== undefined) {
				let result = 0;
				switch(op.operator) {
					case '+': result = v1 + v2; break;
					case '-': result = v1 - v2; break;
					case '*': result = v1 * v2; break;
					case '/': result = v1 / v2; break;
					case '=': break;
					default: throw new "Unsupported operation: " + op.operator;
				}
				if (op.operator !== '=') this.addSolution(op.key, result);
			} else {
				this.queue.push(op);
			}
		}
		return this.solved[id];
	};

	this.addSolution = function(key, value) {
		this.solved[key] = value;
		for (let op of this.queue.filter(x => x.key === key)) {
			const v1 = this.solved[op.op1];
			const v2 = this.solved[op.op2];
			if (v1 !== undefined && v2 === undefined) op.solveForVariable(2);
			else if (v1 === undefined && v2 !== undefined) op.solveForVariable(1);
		}
	};
}

export default Resolver;
