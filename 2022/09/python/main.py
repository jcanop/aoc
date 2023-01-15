from enum import Enum

# --- Constants ---
INPUT_FILE = "../input/input.txt"

def sign(value):
    if value < 0: return -1;
    if value > 0: return 1
    return 0

class Direction(Enum):
    UP    = "U"
    DOWN  = "D"
    LEFT  = "L"
    RIGHT = "R"

    @classmethod
    def from_value(cls, value):
        for e in list(cls):
            if e.value == value: return e
        return None

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def __str__(self):
        return f"({self.x}, {self.y})"

class Rope:
    def __init__(self, size):
        self.list = [ Point(0, 0) for _ in range(size) ];
        self.trail = { Point(0, 0).__str__() }

    def move(self, direction):
        if   direction == Direction.UP:    self.list[0].y -= 1
        elif direction == Direction.DOWN:  self.list[0].y += 1
        elif direction == Direction.LEFT:  self.list[0].x -= 1
        elif direction == Direction.RIGHT: self.list[0].x += 1

        for i in range(1, len(self.list)):
            c = i - 1;
            n = i
            dx = self.list[c].x - self.list[n].x
            dy = self.list[c].y - self.list[n].y
            if abs(dx) < 2 and abs(dy) < 2: return
            self.list[n].x += sign(dx)
            self.list[n].y += sign(dy)

            if i == len(self.list) - 1:
                s = self.list[n].__str__()
                self.trail.add(s)

# --- Values ---
rope_1 = Rope(2)
rope_2 = Rope(10)

# --- Read and parse input file ---
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    direction = Direction.from_value(line[0])
    step = int(line[2:])
    for _ in range(step):
        rope_1.move(direction)
        rope_2.move(direction)

len_1 = len(rope_1.trail)
len_2 = len(rope_2.trail)
print(f"1. Position visited at least once: {len_1:,d}")
print(f"2. Position visited at least once: {len_2:,d}")
