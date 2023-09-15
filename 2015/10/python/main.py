# --- Constants --
INPUT_FILE = "../input/input.txt"

# --- Variables ---
def look_and_say(s):
    ls = [*s]
    res = []
    last = ls[0]
    count = 1
    for i in range(1, len(ls)):
        if ls[i] == last: count += 1
        else:
            res.extend([*str(count)])
            res.append(last)
            count = 1
        last = ls[i]
    res.extend([*str(count)])
    res.append(last)
    return "".join(res)

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
text = file.read().strip()

# --- Puzzle 1 ---
for _ in range(0, 40): text = look_and_say(text)
print(f"1. Length of the result: {len(text):,d}")

# --- Puzzle 2 ---
for _ in range(0, 10): text = look_and_say(text)
print(f"2. Length of the result: {len(text):,d}")
