function to(value) {
	let buffer = "";
	while (value > 0) {
		let overflow = false;
		switch (value % 5) {
			case 0: buffer = "0" + buffer; break;
			case 1: buffer = "1" + buffer; break;
			case 2: buffer = "2" + buffer; break;
			case 3: buffer = "=" + buffer; overflow = true; break;
			case 4: buffer = "-" + buffer; overflow = true; break;
			default: throw "Unreachable!";
		}
		value = Math.floor(value / 5);
		if (overflow) value++;
	}
	return buffer;
}

function from(value) {
	let result = 0;
	for (const [i, c] of value.split("").entries()) {
		let p = Math.pow(5, value.length - i - 1);
		switch (c) {
			case '0': break;
			case '1': result += 1 * p; break;
			case '2': result += 2 * p; break;
			case '=': result -= 2 * p; break;
			case '-': result -= 1 * p; break;
			default: throw "Unreachable!";
		}
	}
	return result;
}

export { to, from };
