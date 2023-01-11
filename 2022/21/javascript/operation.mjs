function Operation(value) {
	const tokens = value.split(/\s+/);
	this.key = tokens[0].substring(0, tokens[0].length - 1);
	this.op1 = tokens[1];
	this.operator = tokens[2].charAt(0);
	this.op2 = tokens[3];

	this.solveForVariable = function(i) {
		if (i === 1) {
			switch(this.operator) {
				case '+': this.operator = '-'; break;
				case '-': this.operator = '+'; break;
				case '*': this.operator = '/'; break;
				case '/': this.operator = '*'; break;
				default: throw "1Unsupported operation: " + this.operator;
			}
			const tmp = this.key;
			this.key = this.op1;
			this.op1 = tmp;
		} else if (i == 2) {
			let swap = false;
			switch(this.operator) {
				case '+': this.operator = '-'; swap = true; break;
				case '-': this.operator = '-'; break;
				case '*': this.operator = '/'; swap = true; break;
				case '/': this.operator = '/'; break;
				default: throw "Unsupported operation: " + this.operator;
			}
			let tmp = this.key;
			this.key = this.op2;
			this.op2 = tmp;
			if (swap) {
				tmp = this.op1;
				this.op1 = this.op2;
				this.op2 = tmp;
			}
		} else {
			throw "Unsupported i: " + i;
		}
	}
}

export default Operation;
