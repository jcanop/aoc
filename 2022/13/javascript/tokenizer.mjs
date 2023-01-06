function createTokenizer(s) {
	return {
		list: Array.from(s.replaceAll(/(^\[|\]$)/g, "")),
		next: function() {
			let count = 0;
			let buffer = "";
			while (this.list.length > 0) {
				const c = this.list.shift();
				if (c === '[') {
					buffer += c;
					count++;
				} else if (c === ',') {
					if (count === 0) return buffer;
					buffer += c;
				} else if (c === ']') {
					buffer += c;
					if (count === 0) return buffer;
					count--;
				} else {
					buffer += c;
				}
			}
			if (buffer.length === 0) return null;
			return buffer;
		}
	};
}

export default createTokenizer;
