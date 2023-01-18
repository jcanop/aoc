from enum import Enum

# --- Tile Enum ---
class Tile(Enum):
    AIR = " "
    ORIGIN = "+"
    ROCK = "#"
    SAND = "o"

# --- Represents a point in the map ---
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        return hash(self.__str__())

    def __str__(self):
        return str(self.x) + "," + str(self.y)

# --- Constants ---
OFFSET = 2
ORIGIN = Point(500, 0)

# --- Represents the map ---
class Map:
    def __init__(self, filename):
        self.data = {}
        self.point = Point(0, 0)

        _list = []
        maxy = 0

        file = open(filename, "r")
        while True:
            line = file.readline()
            if not line: break
            line = line.strip()

            ln = []
            for token in line.split(" -> "):
                s = token.split(",")
                x = int(s[0])
                y = int(s[1])
                ln.append(Point(x, y))
                maxy = max(y, maxy)
            _list.append(ln)

        self.height = maxy + 3
        for ln in _list:
            for i in range(1, len(ln)):
                p1 = ln[i - 1]
                p2 = ln[i]
                x1 = min(p1.x, p2.x)
                x2 = max(p1.x, p2.x)
                y1 = min(p1.y, p2.y)
                y2 = max(p1.y, p2.y)
                for y in range(y1, y2 + 1):
                    for x in range(x1, x2 + 1):
                        self.data[Point(x, y)] = Tile.ROCK

    # --- Get the tile at a position ---
    def _get(self, x, y):
        if y == self.height -1: return Tile.ROCK
        self.point.x = x
        self.point.y = y
        if self.point in self.data: return self.data[self.point]
        if ORIGIN.x == x and ORIGIN.y == y: return Tile.ORIGIN
        return Tile.AIR

    # --- Simulate the fall of sand ---
    def sim(self, puzzle):
        while True:
            x = ORIGIN.x
            y = ORIGIN.y
            while True:
                if puzzle == 1 and y + 1 == self.height - 1: return ""
                if self._get(x, y + 1) == Tile.AIR:
                    y += 1
                    continue
                if self._get(x - 1, y + 1) == Tile.AIR:
                    x -= 1
                    y += 1
                    continue
                if self._get(x + 1, y + 1) == Tile.AIR:
                    x += 1
                    y += 1
                    continue
                self.data[Point(x, y)] = Tile.SAND
                if ORIGIN.x == x and ORIGIN.y == y: return ""
                break

    # --- Count the sand tiles ---
    def count_sand(self):
        return len([ 1 for p in self.data if self.data[p] == Tile.SAND ])

    # --- Prints the map into the console ---
    def print(self):
        _min = 2 ** 31 -1
        _max = 0
        for p in self.data:
            _min = min(_min, p.x)
            _max = max(_max, p.x)

        _min -= OFFSET
        _max += OFFSET + 1

        for y in range(self.height):
            print("|", end="")
            for x in range(_min, _max):
                print(self._get(x, y).value, end="")
            print("|")
