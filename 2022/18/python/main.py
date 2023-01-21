from array import array
from lib import Point

# --- Constants ---
INPUT_FILE = "../input/input.txt"

# --- Searches for a path to the edge of the model ---
def dfs(root, data, closed):
    queue = [root]
    visited = set([root])

    def handle(p, dx, dy, dz):
        if data[p.x + dx][p.y + dy][p.z + dz] == 0:
            n = p.derive(dx, dy, dz)
            if n not in visited:
                queue.append(n)
                visited.add(n)

    while len(queue) > 0:
        p = queue.pop(0)
        if  p.x == 0 or p.x == len(data) - 1 or \
            p.y == 0 or p.y == len(data[0]) - 1 or \
            p.z == 0 or p.z == len(data[0][0]) - 1: return True

        handle(p, -1,  0,  0)
        handle(p,  1,  0,  0)
        handle(p,  0, -1,  0)
        handle(p,  0,  1,  0)
        handle(p,  0,  0, -1)
        handle(p,  0,  0,  1)

    for x in visited: closed.add(x)
    return False

# --- Calculates the surface ---
def calculate(data, queue):
    total = 0
    for p in queue:
        total += 6
        if p.x > 0 and data[p.x - 1][p.y][p.z] == 1: total -= 1
        if p.y > 0 and data[p.x][p.y - 1][p.z] == 1: total -= 1
        if p.z > 0 and data[p.x][p.y][p.z - 1] == 1: total -= 1
        if p.x < len(data) - 1 and data[p.x + 1][p.y][p.z] == 1: total -= 1
        if p.y < len(data[0]) - 1 and data[p.x][p.y + 1][p.z] == 1: total -= 1
        if p.z < len(data[0][0]) - 1 and data[p.x][p.y][p.z + 1] == 1: total -= 1
    return total

# --- Read and parse the input file ---
ls = []
_max = [0] * 3
with open(INPUT_FILE) as file:
    while True:
        line = file.readline()
        if not line: break
        line = line.strip()
        a = [ int(x) for x in line.split(",") ]
        for i in range(len(a)): _max[i] = max(_max[i], a[i])
        ls.append(Point(a[0], a[1], a[2]))

for i in range(len(_max)): _max[i] += 1
data = [[ [0] * _max[2] for _ in range(_max[1]) ] for _ in range(_max[0]) ]
for p in ls: data[p.x][p.y][p.z] = 1

# --- Puzzle 1 ---
total = calculate(data, ls)
print(f"1. Total surface: {total:,d}")

# --- Puzzle 2 ---
closed = set()
for x in range(1, len(data) - 1):
    for y in range(1, len(data[0]) - 1):
        for z in range(1, len(data[0][0]) - 1):
            if data[x][y][z] == 1: continue
            p = Point(x, y, z)
            if p not in closed: dfs(p, data, closed)
for p in closed: data[p.x][p.y][p.z] = 1

total = calculate(data, ls)
print(f"2. Adjusted Adjusted total surface: {total:,d}")
