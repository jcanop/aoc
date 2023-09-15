import re

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"[a-zA-Z]+: capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)$";
MAX = 100;

# --- Functions ---
def not_negative(value):
    if value < 0: return 0
    return value

# --- Variables ---
ingredients = []
max1 = float("-inf")
max2 = float("-inf")

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    cap = re.search(REGEX, line)
    capacity   = int(cap[1])
    durability = int(cap[2])
    flavor     = int(cap[3])
    texture    = int(cap[4])
    calories   = int(cap[5])
    ingredients.append((capacity, durability, flavor, texture, calories))
file.close()

# --- Calculate combinations ---
last = len(ingredients) - 1
ls = [0] * last
ls.append(MAX)

is_done = False
while ls[0] < MAX and not is_done:
    capacity = 0
    durability = 0
    flavor = 0
    texture = 0
    calories = 0
    for i in range(len(ingredients)):
        capacity    += ls[i] * ingredients[i][0]
        durability  += ls[i] * ingredients[i][1]
        flavor      += ls[i] * ingredients[i][2]
        texture     += ls[i] * ingredients[i][3]
        calories    += ls[i] * ingredients[i][4]
    capacity   = not_negative(capacity)
    durability = not_negative(durability)
    flavor     = not_negative(flavor)
    texture    = not_negative(texture)

    total = capacity * durability * flavor * texture
    max1 = max(max1, total)
    if calories == 500: max2 = max(max2, total)

    # --- Calculate next recipe ---
    i = last - 1
    carry = True
    while carry:
        limit = MAX
        if i > 0:
            for j in range(i): limit -= ls[i]
        ls[i] += 1
        if ls[i] > limit:
            ls[i] = 0
            if i > 0: i -= 1
            else:
                is_done = True
                break
        else:
            carry = False
        ls[last] = MAX
        for j in range(last): ls[last] -= ls[j]

# --- Results ---
print(f"1. Total score of the highest-scoring cookie: {max1:,d}")
print(f"2. Total score of the highest-scoring cookie: {max2:,d}")
