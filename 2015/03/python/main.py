# --- Constants --
INPUT_FILE = "../input/input.txt"

# --- Classes ---
class Point:
    def __init__(self):
        self.x = 0
        self.y = 0

    def __str__(self):
        return f"{self.x},{self.y}"

# --- Variables ---
p = Point()
s = Point()
r = Point()
set1 = { str(p) }
set2 = { str(p) }

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
text = file.read().strip()
for i, c in enumerate([*text]):
    if c == '^':
        p.y -= 1
        if i % 2 == 0: s.y -= 1
        else: r.y -= 1
    elif c == 'v':
        p.y += 1
        if i % 2 == 0: s.y += 1
        else: r.y += 1
    elif c == '<':
        p.x -= 1
        if i % 2 == 0: s.x -= 1
        else: r.x -= 1
    elif c == '>':
        p.x += 1
        if i % 2 == 0: s.x += 1
        else: r.x += 1

    set1.add(str(p));
    if i % 2 == 0:
        set2.add(str(s))
    else:
        set2.add(str(r))

# --- Results ---
print(f"1. Houses with at least one present: {len(set1):,d}")
print(f"2. Houses with at least one present: {len(set2):,d}")
