import fs from "node:fs";

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const REGEX = /Hit Points: (\d+).*Damage: (\d+)/;
const PLAYER_HP = 50;
const PLAYER_MANA = 500;
const MAGIC_MISSILE  = { cost:  53, duration: 0, effect: 4 };
const MAGIC_DRAIN    = { cost:  73, duration: 0, effect: 2 };
const MAGIC_SHIELD   = { cost: 113, duration: 6, effect: 7 };
const MAGIC_POISON   = { cost: 173, duration: 6, effect: 3 };
const MAGIC_RECHARGE = { cost: 229, duration: 5, effect: 101 };
const RESULT_LOSE = -1;
const RESULT_WIN = 1;
const RESULT_CONTINUE = 0;

// --- Functions ---
function applyEffects(state) {
	if (state.shield > 0) {
		state.shield--;
		state.playerArmor = (state.shield > 0) ? MAGIC_SHIELD.effect : 0;
	}
	if (state.poison > 0) {
		state.poison--;
		state.bossHP -= MAGIC_POISON.effect;
	}
	if (state.recharge > 0) {
		state.recharge--;
		state.playerMana += MAGIC_RECHARGE.effect;
	}
}

function canCast(state, magic) {
	if (state.playerMana <= magic.cost) return false;
	if (magic === MAGIC_SHIELD) return state.shield === 0;
	if (magic === MAGIC_POISON) return state.poison === 0;
	if (magic === MAGIC_RECHARGE) return state.recharge === 0;
	return true;
}

function cast(state, magic, bossDamage) {
	// --- Players turn ---
	state.playerMana -= magic.cost;
	state.usedMana += magic.cost;
	if (magic === MAGIC_MISSILE) {
		state.bossHP -= magic.effect;
	} else if (magic === MAGIC_DRAIN) {
		state.bossHP -= magic.effect;
		state.playerHP += magic.effect;
	} else if (magic === MAGIC_SHIELD) {
		state.shield = magic.duration;
	} else if (magic === MAGIC_POISON) {
		state.poison = magic.duration;
	} else if (magic === MAGIC_RECHARGE) {
		state.recharge = magic.duration;
	}

	// --- Boss turn ---
	applyEffects(state);
	if (state.bossHP <= 0) return RESULT_WIN;
	const hit = Math.max(bossDamage - state.playerArmor, 1);
	state.playerHP -= hit;
	if (state.playerHP <= 0) return RESULT_LOSE;

	return RESULT_CONTINUE;
}

function play(bossHP, bossDamage, hardmode) {
	const spells = [ MAGIC_DRAIN, MAGIC_SHIELD, MAGIC_POISON, MAGIC_RECHARGE ];
	let queue = [];
	let min = Number.MAX_VALUE;

	// --- Search winning games ---
	let state = {
		playerHP: PLAYER_HP, playerArmor: 0, playerMana: PLAYER_MANA,
		shield: 0, poison: 0, recharge: 0,
		bossHP, usedMana : 0
	};
	queue.push(state);
	while (queue.length > 0) {
		state = queue.pop();

		// --- Ignore worst paths ---
		if (state.usedMana >= min) continue;

		// --- Hard mode ---
		if (hardmode) {
			state.playerHP--;
			if (state.playerHP <= 0) continue;
		}

		// --- Apply player turn effects ---
		applyEffects(state);
		if (state.bossHP <= 0) {
			min = Math.min(min, state.usedMana);
			continue;
		}

		// --- Try every spell except Missile ---
		for (const spell of spells) {
			if (canCast(state, spell)) {
				let clone = Object.assign({}, state);
				const result = cast(clone, spell, bossDamage);
				if (result === RESULT_WIN) min = Math.min(min, clone.usedMana);
				else if (result === RESULT_CONTINUE) queue.push(clone);
			}
		}

		// --- Try cast Missile ---
		if (canCast(state, MAGIC_MISSILE)) {
			const result = cast(state, MAGIC_MISSILE, bossDamage);
			if (result === RESULT_WIN) min = Math.min(min, state.usedMana);
			else if (result === RESULT_CONTINUE) queue.push(state);
		}
	}

	return min;
}

// --- Parse the input file ---
const input = fs.readFileSync(INPUT_FILE, 'utf8').replaceAll("\n", " ");
const cap = input.match(REGEX);
const bossHP = Number(cap[1]);
const bossDamage = Number(cap[2]);

// --- Puzzle 1 ---
let min = play(bossHP, bossDamage, false);
console.log("1. Least amount of mana: ", min);

// --- Puzzle 2 ---
min = play(bossHP, bossDamage, true);
console.log("2. Least amount of mana: ", min);
