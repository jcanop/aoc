from enum import Enum
import copy
import re

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"Hit Points: (\d+).*Damage: (\d+)"
PLAYER_HP = 50
PLAYER_MANA = 500

# --- Enumerations ---
Result = Enum('Result', ['WIN', 'LOSE', 'CONTINUE'])

# --- Classes ---
class Magic:
    def __init__(self, cost, duration, effect):
        self.cost = cost
        self.duration = duration
        self.effect = effect

class State:
    def __init__(self, player_hp, player_mana, boss_hp):
        self.player_hp = player_hp
        self.player_mana = player_mana
        self.player_armor = 0
        self.shield = 0
        self.poison = 0
        self.recharge = 0
        self.boss_hp = boss_hp
        self.used_mana = 0

    def __str__(self):
        return f"{self.player_hp}, {self.player_mana}, {self.player_armor}, {self.shield}, {self.poison}, {self.recharge}, {self.boss_hp}, {self.used_mana}"

# --- Magic Constants ---
MAGIC_MISSILE  = Magic( 53, 0,   4);
MAGIC_DRAIN    = Magic( 73, 0,   2);
MAGIC_SHIELD   = Magic(113, 6,   7);
MAGIC_POISON   = Magic(173, 6,   3);
MAGIC_RECHARGE = Magic(229, 5, 101);

# --- Functions ---
def apply_effects(state):
    if state.shield > 0:
        state.shield -= 1
        state.player_armor = MAGIC_SHIELD.effect if state.shield > 0 else 0
    if state.poison > 0:
        state.poison -= 1
        state.boss_hp -= MAGIC_POISON.effect
    if state.recharge > 0:
        state.recharge -= 1
        state.player_mana += MAGIC_RECHARGE.effect

def can_cast(state, magic):
    if state.player_mana < magic.cost: return False
    if magic == MAGIC_SHIELD: return state.shield == 0
    if magic == MAGIC_POISON: return state.poison == 0
    if magic == MAGIC_RECHARGE: return state.recharge == 0
    return True

def cast(state, magic, boss_damage):
    # --- Players turn ---
    state.player_mana -= magic.cost
    state.used_mana += magic.cost
    if magic == MAGIC_MISSILE:
        state.boss_hp -= magic.effect
    elif magic == MAGIC_DRAIN:
        state.boss_hp -= magic.effect
        state.player_hp += magic.effect
    elif magic == MAGIC_SHIELD:
        state.shield = magic.duration
    elif magic == MAGIC_POISON:
        state.poison = magic.duration
    elif magic == MAGIC_RECHARGE:
        state.recharge = magic.duration
    else:
        raise Exception("Unsupported magic")

    # --- Boss turn ---
    apply_effects(state)
    if state.boss_hp <= 0: return Result.WIN
    hit = max(boss_damage - state.player_armor, 1)
    state.player_hp -= hit
    if state.player_hp <= 0: return Result.LOSE

    return Result.CONTINUE

def play(boss_hp, boss_damage, hardmode):
    spells = [MAGIC_DRAIN, MAGIC_SHIELD, MAGIC_POISON, MAGIC_RECHARGE]
    queue = []
    _min = float("inf")

    # --- Search winning games ---
    state = State(PLAYER_HP, PLAYER_MANA, boss_hp)
    queue.append(state)
    while len(queue) > 0:
        state = queue.pop()

        # --- Ignore worst paths ---
        if state.used_mana >= _min: continue

        # --- Hardmode ---
        if hardmode:
            state.player_hp -= 1
            if state.player_hp <= 0: continue

        # --- Apply player turn effects ---
        apply_effects(state)
        if state.boss_hp <= 0:
            _min = min(_min, state.used_mana)
            continue

        # --- Try every spell except Missile ---
        for spell in spells:
            if can_cast(state, spell):
                clone = copy.deepcopy(state)
                #clone = state.clone()
                result = cast(clone, spell, boss_damage)
                if result == Result.WIN: _min = min(_min, clone.used_mana)
                elif result == Result.CONTINUE: queue.append(clone)

        # --- Try cast Missile ---
        if can_cast(state, MAGIC_MISSILE):
            result = cast(state, MAGIC_MISSILE, boss_damage)
            if result == Result.WIN: _min = min(_min, state.used_mana)
            elif result == Result.CONTINUE: queue.append(state)

    return _min

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
text = file.read().replace("\n", " ").strip()
file.close()
cap = re.search(REGEX, text)
boss_hp = int(cap[1])
boss_damage = int(cap[2])

# --- Puzzle 1 ---
_min = play(boss_hp, boss_damage, False)
print(f"1. Least amount of mana: {_min:,d}")

# --- Puzzle 2 ---
_min = play(boss_hp, boss_damage, True)
print(f"2. Least amount of mana: {_min:,d}")
