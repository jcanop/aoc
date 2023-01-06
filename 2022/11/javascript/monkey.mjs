function createMonkey(cap) {
	let id = Number(cap[1]);
	let items = cap[2].split(/\s*,\s*/).map(x => BigInt(x));
	let operator = cap[3];
	let operand = cap[4] === "old" ? null : BigInt(cap[4]);
	let divisible = BigInt(cap[5]);
	let throwTrue = Number(cap[6]);
	let throwFalse = Number(cap[7]);

	return {
		id, items, operator, operand, divisible, throwTrue, throwFalse,
		inspects: 0, gcd: 0n,
	};
}

function play(monkey, group, relief) {
	let m = group[monkey];
	while (m.items.length > 0) {
		m.inspects++;
		let item = m.items.shift();
		const value = m.operand === null ? item : m.operand;
		switch (m.operator) {
			case "+": item += value; break;
			case "*": item *= value; break;
			default: throw "Unsupported operator: " + m.operator;
		}
		if (relief === 1) {
			item /= 3n;
		} else if (relief === 2) {
			if (m.gcd === 0n)
				m.gcd = group.map(x => x.divisible).reduce((acc, val) => acc * val, 1n);
			item %= m.gcd;
		} else {
			throw "Unsupported relief: " + relief;
		}
		const i = (item % m.divisible === 0n) ? m.throwTrue : m.throwFalse;
		group[i].items.push(item);
	}
}

export { createMonkey, play };
