import re
from lib import Map

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
PUZZLE1_LINE = 2000000
PUZZLE2_MIN = 0
PUZZLE2_MAX = 4000000

# --- Helper functions ---
def in_limit(x, y):
    return PUZZLE2_MIN <= x <= PUZZLE2_MAX and PUZZLE2_MIN <= y <= PUZZLE2_MAX

def calculate_freq(x, y):
    return x * PUZZLE2_MAX + y

# --- Variables ---
_map = Map()
_min = 2 ** 31 - 1
_max = 0
_range = 0

# --- Read and parse the input file ---
file = open(INPUT_FILE, "r")
text = file.read().strip()
for cap in re.findall(REGEX, text):
    sx = int(cap[0]); sy = int(cap[1])
    bx = int(cap[2]); by = int(cap[3])
    _map.add(sx, sy, bx, by)
    _min = min(_min, sx)
    _max = max(_max, sx)
    _range = max(_range, abs(sx - bx) + abs(sy - by))

_min -= _range
_max += _range + 1

# --- Puzzle 1 ---
total = 0
for x in range(_min, _max):
    if x % 10000 == 0:
        prc = round((x - _min) / (_max - _min)  * 100)
        print(f"\rProgress: {prc}%  ", end="")
    if _map.is_in_sensor_range(x, PUZZLE1_LINE) and _map.is_empty(x, PUZZLE1_LINE):
        total += 1

print(f"\r1. Positions that cannot contain a beacon: {total:,d}")

# --- Puzzle 2 ---
freq = 0
total = 0
for p in _map.sensors:
    total += 1
    prc = round(total / len(_map.sensors) * 100)
    print(f"\rProgress: {prc}%  ", end="")

    s = _map.sensors[p]
    r = s.range
    for i in range(r + 1):
        # North -> East
        x = s.x + i
        y = s.y -r + i - 1
        if in_limit(x, y,) and _map.is_in_sensor_range(x, y) == False:
            freq = calculate_freq(x, y)
            break

        # East -> South
        x = s.x + r - i + 1
        y = s.y + i
        if in_limit(x, y) and _map.is_in_sensor_range(x, y) == False:
            freq = calculate_freq(x, y)
            break

        # South -> West
        x = s.x - i
        y = s.y + r - i + 1
        if in_limit(x, y) and _map.is_in_sensor_range(x, y) == False:
            freq = calculate_freq(x, y)
            break

        # West -> North
        x = s.x - r + i - 1
        y = s.y - i
        if in_limit(x, y) and _map.is_in_sensor_range(x, y) == False:
            freq = calculate_freq(x, y)
            break

    if freq > 0: break

print(f"\r2. Frequency: {freq:,d}")
