# --- Constants --
INPUT_FILE = "../input/input.txt"

# --- Map class --
class Map():
    def __init__(self, data):
        self.grid = []
        for line in data.split("\n"):
            self.grid.append([ ord(c) for c in line ])

        self.height = len(self.grid)
        self.width = len(self.grid[0])
        s = ord("S")
        e = ord("E")
        for y in range(len(self.grid)):
            for x in range(len(self.grid[y])):
                if self.grid[y][x] == s:
                    self.start = (x, y)
                    self.grid[y][x] = ord("a")
                if self.grid[y][x] == e:
                    self.end = (x, y)
                    self.grid[y][x] = ord("z")

    # --- Find the shortest path ---
    def find_path(self):
        h = self.height
        w = self.width
        x = self.start[0]
        y = self.start[1]
        a = []
        for _ in range(h): a.append([0] * w);
        a[y][x] = 1
        queue = [ self.start ]
        while len(queue) > 0:
            p = queue.pop(0)
            x = p[0]
            y = p[1]
            if x > 0 and a[y][x - 1] == 0 and self.grid[y][x - 1] <= self.grid[y][x] + 1:
                a[y][x - 1] = a[y][x] + 1
                queue.append((x - 1, y))
            if y > 0 and a[y - 1][x] == 0 and self.grid[y - 1][x] <= self.grid[y][x] + 1:
                a[y - 1][x] = a[y][x] + 1
                queue.append((x, y - 1))
            if x < w - 1 and a[y][x + 1] == 0 and self.grid[y][x + 1] <= self.grid[y][x] + 1:
                a[y][x + 1] = a[y][x] + 1
                queue.append((x + 1, y))
            if y < h - 1 and a[y + 1][x] == 0 and self.grid[y + 1][x] <= self.grid[y][x] + 1:
                a[y + 1][x] = a[y][x] + 1
                queue.append((x, y + 1))

        x = self.end[0]
        y = self.end[1]
        c = a[y][x]
        queue.clear()
        while c > 1:
            if   x > 0     and a[y][x - 1] == c: x -= 1
            elif y > 0     and a[y - 1][x] == c: y -= 1
            elif x < w - 1 and a[y][x + 1] == c: x += 1
            elif y < h - 1 and a[y + 1][x] == c: y += 1
            queue.append((x, y))
            c -= 1

        return queue

    # --- Find shortest posible path ---
    def find_shortest_posible_path(self):
        h = self.height
        w = self.width
        m = 2**31 - 1
        for y in range(h):
            for x in range(w):
                if self.grid[y][x] == ord("a"):
                    self.start = (x, y)
                    steps = len(self.find_path())
                    if steps > 0: m = min(steps, m)
        return m


# --- Read and parse the input file --
file = open(INPUT_FILE, "r")
text = file.read().strip()
_map = Map(text)

# --- Puzzle 1 ---
count = len(_map.find_path())
print(f"1. Fewest steps: {count:,d}")

count = _map.find_shortest_posible_path()
print(f"2. Shortest path: {count:,d}")
