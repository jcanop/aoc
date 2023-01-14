# --- Constants ---
INPUT_FILE = "../input/input.txt"

class Range:
    def __init__(self, min_value, max_value):
        self.min_value = min_value
        self.max_value = max_value

    # --- Parse a range from a String value ---
    @staticmethod
    def parse(value):
        a = value.split("-")
        mn = int(a[0])
        mx = int(a[1])
        return Range(mn, mx)

    # --- Checks if this range fully contains another range ---
    def fully_contains(self, r):
        return self.min_value <= r.min_value and r.max_value <= self.max_value

    # --- Checks if this range overlaps with another range ---
    def overlap(self, r):
        return \
            (r.min_value >= self.min_value and r.min_value <= self.max_value) or \
            (r.max_value >= self.min_value and r.max_value <= self.max_value) or \
            (self.min_value >= r.min_value and self.min_value <= r.max_value) or \
            (self.max_value >= r.min_value and self.max_value <= r.max_value)

# --- Read and parse the input file ---
ls = []
file = open(INPUT_FILE)
while True:
    line = file.readline()
    if not line:
        break

    line = line.strip()
    a = line.split(",")
    ls.append((Range.parse(a[0]), Range.parse(a[1])))

# --- Puzzle 1 ---
total = len([ 1 for (r1, r2) in ls if r1.fully_contains(r2) or r2.fully_contains(r1) ])
print(f"1. Total of ranges that fully contains another: {total:,d}")

# --- Puzzle 2 ---
total = len([ 1 for (r1, r2) in ls if r1.overlap(r2) ])
print(f"2. Total of ranges that overlaps: {total:,d}")
