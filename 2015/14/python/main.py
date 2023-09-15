import re

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"^.+ can fly (\d+) km\/s for (\d+) seconds, but then must rest for (\d+) seconds\.$"
TIME = 2503

# --- Classes ---
class Reindeer:
    def __init__(self, speed, fly, rest):
        self.speed = speed
        self.fly = fly
        self.rest = rest
        self.distance = 0
        self.fly_count = 0
        self.rest_count = 0
        self.points = 0

    def tick(self):
        if self.fly_count > 0:
            self.fly_count -= 1
            self.distance += self.speed
        elif self.rest_count > 0:
            self.rest_count -= 1
        else:
            self.fly_count = self.fly - 1
            self.rest_count = self.rest
            self.distance += self.speed

# --- Variables ---
ls = []

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    cap = re.search(REGEX, line)
    s = int(cap[1])
    f = int(cap[2])
    r = int(cap[3])
    ls.append(Reindeer(s, f, r))
file.close()

# --- Simulate race ---
for _ in range(TIME):
    for r in ls: r.tick()
    _max = max(ls, key=lambda x: x.distance).distance
    for r in ls:
        if r.distance == _max: r.points += 1

# --- Puzzle 1 ---
_max = max(ls, key=lambda x: x.distance).distance
print(f"1. Winning reindeer distance: {_max:,d}")

# --- Puzzle 2 ---
_max = max(ls, key=lambda x: x.points).points
print(f"2. Winning reindeer points: {_max:,d}")
