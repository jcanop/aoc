from array import array

# --- Constants ---
SHAPES = [
    [(0, 0), (1, 0), (2, 0), (3, 0)],
    [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
    [(2, 0), (2, 1), (0, 2), (1, 2), (2, 2)],
    [(0, 0), (0, 1), (0, 2), (0, 3)],
    [(0, 0), (1, 0), (0, 1), (1, 1)]
]

WIDTH = 7
MARGIN_LEFT = 2
MARGIN_TOP = 3
AIR = '.'
ROCK = '@'
SAND = '#'
LEFT = '<'
RIGHT = '>'
DOWN = 'V'

class Chamber:
    def __init__(self):
        self.rows = []
        self.shape_len = 0

    def find_from_top(self, tile):
        n = len(self.rows) - 1
        for (i, row) in enumerate(reversed(self.rows)):
            if tile in row: return n - i
        return None

    def height(self):
        return self.find_from_top(SAND) - 1

    def add_shape(self, shape):
        # --- Remove extra rows ---
        top = self.find_from_top(SAND)
        if top is not None:
            n = len(self.rows) - top - 1
            for _ in range(n): self.rows.pop(len(self.rows) - 1)

        # --- Add rows ---
        self.shape_len = shape[len(shape) - 1][1] + 1
        n = MARGIN_TOP + self.shape_len
        for _ in range(n): self.rows.append([AIR] * WIDTH)

        # --- Add the shape ---
        height = len(self.rows) - 1
        for (x, y) in shape:
            self.rows[height - y][MARGIN_LEFT + x] = ROCK

    def can_move(self, direction):
        i = self.find_from_top(ROCK)
        if direction == DOWN:
            i -= self.shape_len - 1
            if i == 0: return False
            for y in range(i, i + self.shape_len):
                up = self.rows[y]
                down = self.rows[y - 1]
                for x in range(WIDTH):
                    if up[x] == ROCK and down[x] == SAND: return False
        elif direction == LEFT:
            for y in reversed(range(i + 1 - self.shape_len, i + 1)):
                row = self.rows[y]
                if row[0] == ROCK: return False
                for x in range(1, WIDTH):
                    if row[x] == ROCK:
                        if row[x - 1] != AIR: return False
                        break
        elif direction == RIGHT:
            for y in reversed(range(i + 1 - self.shape_len, i + 1)):
                row = self.rows[y]
                if row[WIDTH - 1] == ROCK: return False
                for x in reversed(range(WIDTH - 1)):
                    if row[x] == ROCK:
                        if row[x + 1] != AIR: return False
                        break
        else:
            raise Exception("Unsupported direction: " + direction)
        return True


    def move(self, direction):
        if self.can_move(direction):
            if direction == DOWN:
                row = self.find_from_top(ROCK) - (self.shape_len - 1)
                for y in range(row, row + self.shape_len):
                    for x in range(WIDTH):
                        if self.rows[y][x] == ROCK:
                            self.rows[y - 1][x] = self.rows[y][x]
                            self.rows[y][x] = AIR
            elif direction == LEFT:
                i = self.find_from_top(ROCK)
                for y in reversed(range(i + 1 - self.shape_len, i + 1)):
                    for x in range(WIDTH - 1):
                        if self.rows[y][x + 1] == ROCK:
                            self.rows[y][x] = self.rows[y][x + 1]
                            self.rows[y][x + 1] = AIR
            elif direction == RIGHT:
                i = self.find_from_top(ROCK)
                for y in reversed(range(i + 1 - self.shape_len, i + 1)):
                    for x in reversed(range(1, WIDTH)):
                        if self.rows[y][x - 1] == ROCK:
                            self.rows[y][x] = self.rows[y][x - 1]
                            self.rows[y][x - 1] = AIR
            else:
                raise Exception("Unsupported direction: " + direction)
        elif direction == DOWN:
            i = self.find_from_top(ROCK) - (self.shape_len - 1)
            for y in range(i, i + self.shape_len):
                for x in range(WIDTH):
                    if self.rows[y][x] == ROCK: self.rows[y][x] = SAND
            return False
        return True

    def move_down(self):
        return self.move(DOWN)

    def print(self):
        for row in reversed(self.rows):
            print("|", end="")
            for c in row: print(c, end="")
            print("|")
        print("+", end="")
        for _ in range(WIDTH): print("-", end="")
        print("+")
