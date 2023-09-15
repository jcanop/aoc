from itertools import permutations
import re

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"^(.+) to (.+) = (\d+)$"

# --- Variables ---
places = set()
distances = {}
min_distance = float("inf")
max_distance = float("-inf")

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    cap = re.search(REGEX, line)
    places.add(cap[1])
    places.add(cap[2])
    distances[cap[1] + "-" + cap[2]] = int(cap[3])
    distances[cap[2] + "-" + cap[1]] = int(cap[3])
file.close()

for p in permutations(places):
    distance = 0
    for i in range(0, len(p) - 1):
        distance += distances[p[i] + "-" + p[i + 1]]

    min_distance = min(min_distance, distance)
    max_distance = max(max_distance, distance)


# --- Result ---
print(f"1. Min distance: {min_distance:,d}")
print(f"2. Max distance: {max_distance:,d}")
