use regex::Regex;

// --- Constants --
const INPUT_FILE: &'static str = "../input/input.txt";
const PARSER_REGEX: &'static str = r"Hit Points: (\d+).*Damage: (\d+).*Armor: (\d+)";
const PLAYER_HP: usize = 100;

// --- Structs ---
#[derive(Debug)]
struct Item {
    cost: usize,
    damage: usize,
    armor: usize
}

impl Item {
    fn new(cost: usize, damage: usize, armor: usize) -> Self {
        Item { cost, damage, armor }
    }
}

#[derive(Debug)]
struct Player {
    hp: usize,
    damage: usize,
    armor: usize,
    cost: usize
}

impl Player {
    fn new(hp: usize, weapon: &Item, armor: Option<&Item>, ring1: Option<&Item>, ring2: Option<&Item>) -> Self {
        let mut cost = weapon.cost;
        let mut damage = weapon.damage;
        let mut shield = weapon.armor;
        if let Some(i) = armor {
            cost += i.cost;
            damage += i.damage;
            shield += i.armor;
        }
        if let Some(i) = ring1 {
            cost += i.cost;
            damage += i.damage;
            shield += i.armor;
        }
        if let Some(i) = ring2 {
            cost += i.cost;
            damage += i.damage;
            shield += i.armor;
        }
        Self { hp, cost, damage, armor: shield }
    }

    fn boss(hp: usize, damage: usize, armor: usize) -> Self {
        Self { hp, damage, armor, cost: 0 }
    }
}

// --- Plays the game, returns true if the player wins, otherwise false ---
fn play(player: &Player, boss: &Player) -> bool {
    let mut ph = player.hp as isize;
    let mut bh = boss.hp as isize;
    while ph > 0 {
        let hit: isize = player.damage as isize - boss.armor as isize;
        bh -= if hit <= 0 { 1 } else { hit };
        if bh <= 0 { return true; }

        let hit: isize = boss.damage as isize - player.armor as isize;
        ph -= if hit <= 0 { 1 } else { hit };
    }
    false
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut min = std::usize::MAX;
    let mut max = std::usize::MIN;

    // --- Shop Items ---
    let weapons = vec![
        Item::new( 8, 4, 0), Item::new(10, 5, 0), Item::new(25, 6, 0),
        Item::new(40, 7, 0), Item::new(74, 8, 0)
    ];
    let armors = vec![
        Item::new(13, 0, 1), Item::new(31, 0, 2), Item::new(53, 0, 3),
        Item::new(75, 0, 4), Item::new(102, 0, 5)
    ];
    let rings = vec![
        Item::new(25, 1, 0), Item::new(50, 2, 0), Item::new(100, 3, 0),
        Item::new(20, 0, 1), Item::new(40, 0, 2), Item::new( 80, 0, 3)
    ];

    // --- Read and parse the input file ---
    let regex = Regex::new(PARSER_REGEX)?;
    let text = std::fs::read_to_string(INPUT_FILE)?.replace("\n", " ");
    let cap = regex.captures(&text).unwrap();
    let hp: usize = cap[1].parse()?;
    let damage: usize = cap[2].parse()?;
    let armor: usize  = cap[3].parse()?;
    let boss = Player::boss(hp, damage, armor);

    // --- Combinations ---
    for w in 0 .. weapons.len() {
        for a in 0 ..= armors.len() {
            for r1 in 0 ..= rings.len() {
                for r2 in 0 ..= rings.len() {
                    if r1 > 0 && r1 == r2 { continue; }
                    let weapon = &weapons[w];
                    let armor = if a  == 0 { None } else { Some(&armors[a - 1]) };
                    let ring1 = if r1 == 0 { None } else { Some(&rings[r1 - 1]) };
                    let ring2 = if r2 == 0 { None } else { Some(&rings[r2 - 1]) };
                    let player = Player::new(PLAYER_HP, weapon, armor, ring1, ring2);
                    if play(&player, &boss) {
                        min = std::cmp::min(min, player.cost);
                    } else {
                        max = std::cmp::max(max, player.cost);
                    }
                }
            }
        }
    }

    // --- Puzzle 1 ---
    println!("1. Least amount of gold and win: {}", min);

    // --- Puzzle 2 ---
    println!("2. Most amount of gold and lose: {}", max);

    Ok(())
}
