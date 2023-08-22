use enum_derive_list::AllVariants;
use regex::Regex;

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = r"Hit Points: (\d+).*Damage: (\d+)";
const PLAYER_HP: isize = 50;
const PLAYER_MANA: usize = 500;
const MAGIC_MISSILE: Magic  = Magic { cost:  53, duration: 0, effect: 4 };
const MAGIC_DRAIN: Magic    = Magic { cost:  73, duration: 0, effect: 2 };
const MAGIC_SHIELD: Magic   = Magic { cost: 113, duration: 6, effect: 7 };
const MAGIC_POISON: Magic   = Magic { cost: 173, duration: 6, effect: 3 };
const MAGIC_RECHARGE: Magic = Magic { cost: 229, duration: 5, effect: 101 };

// --- Enums ---
#[derive(Debug, PartialEq)]
enum PlayResult { Win, Lose, Continue }

#[derive(AllVariants, Debug, PartialEq)]
enum Spell { Missile, Drain, Shield, Poison, Recharge }

impl Spell {
    fn magic(&self) -> &Magic {
        match self {
            Spell::Missile  => &MAGIC_MISSILE,
            Spell::Drain    => &MAGIC_DRAIN,
            Spell::Shield   => &MAGIC_SHIELD,
            Spell::Poison   => &MAGIC_POISON,
            Spell::Recharge => &MAGIC_RECHARGE
        }
    }
}

// --- Structs ---
#[derive(Debug)]
struct Magic {
    cost: usize,
    duration: usize,
    effect: usize
}

#[derive(Debug, Clone)]
struct State {
    p_hp: isize,
    p_armor: usize,
    p_mana: usize,
    shield: usize,
    poison: usize,
    recharge: usize,
    b_hp: isize,
    used_mana: usize
}

impl State {
    fn new(b_hp: isize) -> Self {
        State { p_hp: PLAYER_HP, p_armor: 0, p_mana: PLAYER_MANA, shield: 0, poison: 0, recharge: 0, b_hp, used_mana: 0 }
    }
}

// --- Checks if the player can cast a spell ---
fn can_cast(state: &State, spell: &Spell) -> bool {
    if state.p_mana < spell.magic().cost { return false; }
    match spell {
        Spell::Shield   => state.shield == 0,
        Spell::Poison   => state.poison == 0,
        Spell::Recharge => state.recharge == 0,
        Spell::Missile | Spell::Drain => true
    }
}

// --- Apply the current active effects ---
fn apply_effects(state: &mut State) {
    if state.shield > 0 {
        state.shield -= 1;
        state.p_armor = if state.shield > 0 { Spell::Shield.magic().effect } else { 0 };
    }
    if state.poison >  0 {
        state.poison -= 1;
        state.b_hp -= Spell::Poison.magic().effect as isize;
    }
    if state.recharge > 0 {
        state.recharge -= 1;
        state.p_mana += Spell::Recharge.magic().effect;
    }
}

// --- Cast a spell and simulate boss turn ---
fn cast(state: &mut State, spell: &Spell, boss_damage: usize) -> PlayResult {
    // --- Player turn ---
    let magic = spell.magic();
    state.p_mana -= magic.cost;
    state.used_mana += magic.cost;
    match spell {
        Spell::Missile => state.b_hp -= magic.effect as isize,
        Spell::Drain => {
            state.b_hp -= magic.effect as isize;
            state.p_hp += magic.effect as isize;
        },
        Spell::Shield   => state.shield = magic.duration,
        Spell::Poison   => state.poison = magic.duration,
        Spell::Recharge => state.recharge = magic.duration
    }

    // --- Boss turn ---
    apply_effects(state);
    if state.b_hp <= 0 { return PlayResult::Win; }

    let hit = std::cmp::max(boss_damage as isize - state.p_armor as isize, 1);
    state.p_hp -= hit;
    if state.p_hp <= 0 { return PlayResult::Lose; }

    PlayResult::Continue
}

// --- Play the game ---
fn play(b_hp: isize, b_damage: usize, hardmode: bool) -> usize {
    let mut queue: Vec<State> = Vec::new();
    let mut min = std::usize::MAX;

    // --- Search winning games ---
    queue.push(State::new(b_hp));
    while queue.len() > 0 {
        let mut state = queue.pop().unwrap();

        // --- Ignore worst paths ---
        if state.used_mana >= min { continue; }

        // --- Hard mode ---
        if hardmode {
            state.p_hp -= 1;
            if state.p_hp <= 0 { continue; }
        }

        // --- Apply player turn effects ---
        apply_effects(&mut state);
        if state.b_hp <= 0 {
            min = std::cmp::min(min, state.used_mana);
            continue;
        }

        // --- Try every spell except Missile ---
        for spell in Spell::all_variants() {
            if spell == &Spell::Missile { continue; }
            if can_cast(&state, spell) {
                let mut clone = state.clone();
                let result = cast(&mut clone, spell, b_damage);
                match result {
                    PlayResult::Win      => min = std::cmp::min(min, clone.used_mana),
                    PlayResult::Lose     => (),
                    PlayResult::Continue => queue.push(clone)
                }
            }
        }

        // --- Try cast Missile ---
        if can_cast(&state, &Spell::Missile) {
            match cast(&mut state, &Spell::Missile, b_damage) {
                PlayResult::Win      => min = std::cmp::min(min, state.used_mana),
                PlayResult::Lose     => (),
                PlayResult::Continue => queue.push(state)
            }
        }
    }
    min
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Read and parse the input file ---
    let regex = Regex::new(PARSER_REGEX)?;
    let text = std::fs::read_to_string(INPUT_FILE)?.replace("\n", " ");
    let cap = regex.captures(&text).unwrap();
    let health: isize = cap[1].parse()?;
    let damage: usize = cap[2].parse()?;

    let min = play(health, damage, false);
    println!("1. Least amount of mana: {}", min);

    let min = play(health, damage, true);
    println!("2. Least amount of mana: {}", min);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cast_1() {
        let damage = 8;
        let mut state = State::new(13);
        state.p_hp = 10;
        state.p_mana = 250;
        apply_effects(&mut state);
        let result = cast(&mut state, &Spell::Poison, damage);
        assert_eq!(result, PlayResult::Continue);
        assert_state(&state, 2, 0, 77, 0, 5, 0, 10, 173);
        apply_effects(&mut state);
        let result = cast(&mut state, &Spell::Missile, damage);
        assert_eq!(result, PlayResult::Win);
        assert_state(&state, 2, 0, 24, 0, 3, 0, 0, 173 + 53);
    }

    #[test]
    fn test_cast_2() {
        let damage = 8;
        let mut state = State::new(14);
        state.p_hp = 10;
        state.p_mana = 250;
        apply_effects(&mut state);
        let result = cast(&mut state, &Spell::Recharge, damage);
        assert_eq!(result, PlayResult::Continue);
        assert_state(&state, 2, 0, 122, 0, 0, 4, 14, 229);
        apply_effects(&mut state);
        let result = cast(&mut state, &Spell::Shield, damage);
        assert_eq!(result, PlayResult::Continue);
        assert_state(&state, 1, 7, 211, 5, 0, 2, 14, 342);
        apply_effects(&mut state);
        let result = cast(&mut state, &Spell::Drain, damage);
        assert_eq!(result, PlayResult::Continue);
        assert_state(&state, 2, 7, 340, 3, 0, 0, 12, 415);
        apply_effects(&mut state);
        let result = cast(&mut state, &Spell::Poison, damage);
        assert_eq!(result, PlayResult::Continue);
        assert_state(&state, 1, 7, 167, 1, 5, 0, 9, 588);
        apply_effects(&mut state);
        let result = cast(&mut state, &Spell::Missile, damage);
        assert_eq!(result, PlayResult::Win);
        assert_state(&state, 1, 0, 114, 0, 3, 0, -1, 641);
    }

    fn assert_state(state: &State, p_hp: isize, p_armor: usize, p_mana: usize, shield: usize, poison: usize, recharge: usize, b_hp: isize, used_mana: usize) {
        assert_eq!(state.p_hp, p_hp);
        assert_eq!(state.p_armor, p_armor);
        assert_eq!(state.p_mana, p_mana);
        assert_eq!(state.shield, shield);
        assert_eq!(state.poison, poison);
        assert_eq!(state.recharge, recharge);
        assert_eq!(state.b_hp, b_hp);
        assert_eq!(state.used_mana, used_mana);
    }
}
