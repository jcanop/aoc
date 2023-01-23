import re

# --- Constants ---
EMPTY  = ' '
OPEN   = '.'
WALL   = '#'
RIGHT  = 'R'
LEFT   = 'L'
EAST   = 0
SOUTH  = 1
WEST   = 2
NORTH  = 3

# --- Layout ---
class Layout:
    def __init__(self, text):
        self.grid = [line  for line in text.split("\n")]
        _max = max([ len(line) for line  in self.grid ])
        for y in range(len(self.grid)):
            for _ in range(len(self.grid[y]), _max): self.grid[y] += EMPTY

        self.height = len(self.grid)
        self.width = len(self.grid[0])
        (self.x, self.y) = _find_start(self.grid, self.height, self.width)
        self.d = EAST

    def path(self, path, func):
        for c in re.findall(r"(\d+|R|L)", path):
            if   c == LEFT:
                if self.d == 0: self.d = 3
                else: self.d -= 1
            elif c == RIGHT:
                if self.d == 3: self.d = 0
                else: self.d += 1
            else: func(self, int(c))

    def get_password(self):
        return 1000 * (self.y + 1) + 4 * (self.x + 1) + self.d

def _find_start(g, h, w):
    for y in range(h):
        for x in range(w):
            if g[y][x] == OPEN: return (x, y)
    raise Exception("Starting point not found!")

# --- Puzzle 1: Grid ---
def move_grid(layout, steps):
    x = layout.x
    y = layout.y

    while steps > 0:
        if layout.d == NORTH:
            if y == 0: y = layout.height - 1
            else: y -= 1
        elif layout.d == SOUTH:
            if y == layout.height - 1: y = 0
            else: y += 1
        elif layout.d == WEST:
            if x == 0: x = layout.width - 1
            else: x -= 1
        elif layout.d == EAST:
            if x == layout.width - 1: x = 0
            else: x += 1

        if layout.grid[y][x] == WALL: break
        if layout.grid[y][x] == OPEN: steps -= 1; layout.x = x; layout.y = y

# --- Puzzle 2: Cube ---
def move_cube(layout, steps):
    while steps > 0:
        x, y, d = layout.x, layout.y, layout.d

        if   d == NORTH: y -= 1
        elif d == SOUTH: y += 1
        elif d == WEST:  x -= 1
        elif d == EAST:  x += 1
        else: raise Exception("Unreacheable")

        if x < 0 or x >= layout.width or y < 0 or y >= layout.height or \
                layout.grid[y][x] == EMPTY:
            r = layout.y // 50
            c = layout.x // 50
            d = layout.d

            if   r == 0 and c == 1 and d == NORTH: r = 3; c = 0; d = EAST
            elif r == 0 and c == 1 and d == WEST:  r = 2; c = 0; d = EAST
            elif r == 0 and c == 2 and d == NORTH: r = 3; c = 0; d = NORTH
            elif r == 0 and c == 2 and d == EAST:  r = 2; c = 1; d = WEST
            elif r == 0 and c == 2 and d == SOUTH: r = 1; c = 1; d = WEST
            elif r == 1 and c == 1 and d == EAST:  r = 0; c = 2; d = NORTH
            elif r == 1 and c == 1 and d == WEST:  r = 2; c = 0; d = SOUTH
            elif r == 2 and c == 0 and d == NORTH: r = 1; c = 1; d = EAST
            elif r == 2 and c == 0 and d == WEST:  r = 0; c = 1; d = EAST
            elif r == 2 and c == 1 and d == EAST:  r = 0; c = 2; d = WEST
            elif r == 2 and c == 1 and d == SOUTH: r = 3; c = 0; d = WEST
            elif r == 3 and c == 0 and d == EAST:  r = 2; c = 1; d = NORTH
            elif r == 3 and c == 0 and d == SOUTH: r = 0; c = 2; d = SOUTH
            elif r == 3 and c == 0 and d == WEST:  r = 0; c = 1; d = SOUTH
            else: raise Exception("Unreacheable!")

            ci = layout.x % 50
            ri = layout.y % 50
            if   layout.d == NORTH: i = ci
            elif layout.d == SOUTH: i = 49 - ci
            elif layout.d == WEST:  i = 49 - ri
            elif layout.d == EAST:  i = ri
            else: raise Exception("Unreacheable!")

            if   d == NORTH: rn =     49; cn = i
            elif d == SOUTH: rn =      0; cn = 49 - i
            elif d == WEST:  rn = 49 - i; cn = 49
            elif d == EAST:  rn =      i; cn = 0
            else: raise Exception("Unreacheable!")

            x = 50 * c + cn
            y = 50 * r + rn

        if layout.grid[y][x] == WALL: break
        if layout.grid[y][x] == OPEN: steps -= 1; layout.x = x; layout.y = y; layout.d = d
