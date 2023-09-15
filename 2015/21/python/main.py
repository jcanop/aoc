import re

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"Hit Points: (\d+).*Damage: (\d+).*Armor: (\d+)"
PLAYER_HP = 100;

# --- Classes ---
class Item:
    def __init__(self, cost, damage, armor):
        self.cost = cost
        self.damage = damage
        self.armor = armor

class Player:
    def __init__(self, hp, weapon, armor, ring1, ring2):
        self.hp = hp
        self.cost   = weapon.cost
        self.damage = weapon.damage
        self.armor  = weapon.armor
        if armor is not None:
            self.cost   += armor.cost
            self.damage += armor.damage
            self.armor  += armor.armor
        if ring1 is not None:
            self.cost   += ring1.cost
            self.damage += ring1.damage
            self.armor  += ring1.armor
        if ring2 is not None:
            self.cost   += ring2.cost
            self.damage += ring2.damage
            self.armor  += ring2.armor

class Boss:
    def __init__(self, hp, damage, armor):
        self.hp = hp
        self.damage = damage
        self.armor = armor

# --- Shop items ---
WEAPONS = [Item( 8, 4, 0), Item(10, 5, 0), Item( 25, 6, 0), Item(40, 7, 0), Item( 74, 8, 0)]
ARMORS  = [Item(13, 0, 1), Item(31, 0, 2), Item( 53, 0, 3), Item(75, 0, 4), Item(102, 0, 5)]
RINGS   = [Item(25, 1, 0), Item(50, 2, 0), Item(100, 3, 0), Item(20, 0, 1), Item( 40, 0, 2), Item(80, 0, 3)]

# --- Functions ---
def play(player, boss):
    ph = player.hp
    bh = boss.hp
    while ph > 0:
        hit = player.damage - boss.armor
        bh -= max(hit, 1)
        if bh <= 0: return True

        hit = boss.damage - player.armor
        ph -= max(hit, 1)
    return False

# --- Variables ---
_min = float("inf")
_max = float("-inf")

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
text = file.read().strip()
text = text.replace("\n", " ")
file.close()

cap = re.search(REGEX, text)
hp = int(cap[1])
damage = int(cap[2])
armor = int(cap[3])
boss = Boss(hp, damage, armor)

# --- Combinations ---
for weapon in WEAPONS:
    for a in range(-1, len(ARMORS)):
        for r1 in range(-1, len(RINGS)):
            for r2 in range(-1, len(RINGS)):
                if r1 > -1 and r1 == r2: continue
                armor = None if a  == -1 else ARMORS[a]
                ring1 = None if r1 == -1 else RINGS[r1]
                ring2 = None if r2 == -1 else RINGS[r2]
                player = Player(PLAYER_HP, weapon, armor, ring1, ring2)
                if play(player, boss):
                    _min = min(_min, player.cost)
                else:
                    _max = max(_max, player.cost)

# --- Puzzle 1 ---
print(f"1. Least amount of gold and win: {_min:,d}")

# --- Puzzle 2 ---
print(f"2. Most amount of gold and lose: {_max:,d}")
