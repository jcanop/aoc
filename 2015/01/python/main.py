# --- Constants ---
INPUT_FILE = "../input/input.txt"

# --- Read input file ---
file = open(INPUT_FILE, mode='r')
data = file.read();
file.close()

# --- Puzzle 1 & 2 ---
floor = 0
pos = -1
for i, c in enumerate([*data]):
    if c == ')':
        floor -= 1
    else:
        floor += 1
    if pos == -1 and floor == -1:
        pos = i + 1;

# --- Results ---
print(f"1. Floor: {floor:,d}")
print(f"2. Position: {pos:,d}")
