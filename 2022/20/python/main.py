# --- Constants ---
INPUT_FILE = "../input/input.txt";
KEY = 811589153;
MIX_COUNT = 10;
OFFSET_X = 1000
OFFSET_Y = 2000
OFFSET_Z = 3000


# --- Mix the order of numbers ---
def mix(ls):
    _len = len(ls) - 1
    for _id in range(len(ls)):
        i = next((i for i, item in enumerate(ls) if item[0] == _id), -1)
        if ls[i][1] == 0: continue
        item = ls.pop(i)
        j = (i + item[1]) % _len
        if j < 0: j = _len + j
        if j == 0: j = _len
        ls.insert(j, item)

# --- Find and sum the 3 coordinates ---
def sum_coord(ls):
    i = next((i for i, item in enumerate(ls) if item[1] == 0), -1)
    _len = len(ls)
    x = ls[((i + OFFSET_X) % _len)][1]
    y = ls[((i + OFFSET_Y) % _len)][1]
    z = ls[((i + OFFSET_Z) % _len)][1]
    return x + y + z

# --- Read and parse the input file ---
with open(INPUT_FILE) as file:
    text = file.read().strip()

# --- Puzzle 1 ---
ls = [ (i, int(x)) for i, x in enumerate(text.split("\n")) ]
mix(ls)
total = sum_coord(ls)
print(f"1. The sum of the coordinates: {total:,d}")

# --- Puzzle 1 ---
ls = [ (i, int(x) * KEY) for i, x in enumerate(text.split("\n")) ]
for _ in range(MIX_COUNT): mix(ls)
total = sum_coord(ls)
print(f"2. The sum of the coordinates: {total:,d}")
