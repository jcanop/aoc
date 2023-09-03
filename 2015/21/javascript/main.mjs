import fs from "node:fs";

// --- Helper function ---
let Item = (cost, damage, armor) => { return {cost, damage, armor}; };

// --- Constants ---
const INPUT_FILE = "../input/input.txt";
const REGEX = /Hit Points: (\d+).*Damage: (\d+).*Armor: (\d+)/
const WEAPONS = [Item( 8, 4, 0), Item(10, 5, 0), Item( 25, 6, 0), Item(40, 7, 0), Item( 74, 8, 0)];
const ARMORS  = [Item(13, 0, 1), Item(31, 0, 2), Item( 53, 0, 3), Item(75, 0, 4), Item(102, 0, 5)];
const RINGS   = [Item(25, 1, 0), Item(50, 2, 0), Item(100, 3, 0), Item(20, 0, 1), Item( 40, 0, 2), Item(80, 0, 3)];
const PLAYER_HP = 100;

// --- Functions ---
function Player(hp, weapon, armor, ring1, ring2) {
	let cost = weapon.cost;
	let damage = weapon.damage;
	let shield = weapon.armor;
	if (armor !== null) {
		cost += armor.cost;
		damage += armor.damage;
		shield += armor.armor;
	}
	if (ring1 !== null) {
		cost += ring1.cost;
		damage += ring1.damage;
		shield += ring1.armor;
	}
	if (ring2 !== null) {
		cost += ring2.cost;
		damage += ring2.damage;
		shield += ring2.armor;
	}
	return {hp, damage, armor: shield, cost };
}

function play(player, boss) {
	let ph = player.hp;
	let bh = boss.hp;
	while (ph > 0) {
		let hit = player.damage - boss.armor;
		bh -= (hit <= 0) ? 1 : hit;
		if (bh <= 0) return true;

		hit = boss.damage - player.armor;
		ph -= (hit <= 0) ? 1 : hit;
	}
	return false;
}

// --- Variables ---
let min = Number.MAX_VALUE;
let max = Number.MIN_VALUE;

// --- Parse the input file ---
const input = fs.readFileSync(INPUT_FILE, 'utf8').replaceAll("\n", " ");
const cap = input.match(REGEX);
const hp = Number(cap[1]);
const damage = Number(cap[2]);
const armor = Number(cap[3]);
const boss = {hp, damage, armor};

// --- Combinations ---
for (let w = 0; w < WEAPONS.length; w++) {
	for (let a = 0; a <= ARMORS.length; a++) {
		for (let r1 = 0; r1 <= RINGS.length; r1++) {
			for (let r2 = 0; r2 <= RINGS.length; r2++) {
				if (r1 > 0 && r1 === r2) continue;
				const weapon = WEAPONS[w];
				const armor = (a === 0) ? null : ARMORS[a - 1];
				const ring1 = (r1 === 0) ? null : RINGS[r1 - 1];
				const ring2 = (r2 === 0) ? null : RINGS[r2 - 1];
				const player = Player(PLAYER_HP, weapon, armor, ring1, ring2);
				if (play(player, boss)) {
					min = Math.min(min, player.cost);
				} else {
					max = Math.max(max, player.cost);
				}
			}
		}
	}
}

// --- Puzzle 1 ---
console.log("1. Least amount of gold and win: ", min.toLocaleString());

// --- Puzzle 2 ---
console.log("2. Most amount of gold and lose: ", max.toLocaleString());
