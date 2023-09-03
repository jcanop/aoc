// https://stackoverflow.com/questions/66108781/finding-all-permutations-of-array-elements-as-concatenated-strings

export default function* permutations (t) {
	if (t.length < 2)
		yield t
	else
		for (const p of permutations(t.slice(1)))
			for (const r of rotations(p, t[0]))
				yield r
}

function* rotations (t, v) {
	if (t.length == 0)
		yield [v]
	else
		yield *chain (
			[[v, ...t]],
			map(rotations(t.slice(1), v), r => [t[0], ...r])
		)
}

function* map (t, f) {
	for (const e of t) yield f(e)
}

function* chain (...ts) {
	for (const t of ts)
		for (const e of t)
			yield e
}
