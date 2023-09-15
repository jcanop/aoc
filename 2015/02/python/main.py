import re

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"(\d+)x(\d+)x(\d+)"

# --- Variables ---
total = 0
ribbon = 0

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
text = file.read().strip()
for cap in re.findall(REGEX, text):
    l = int(cap[0])
    w = int(cap[1])
    h = int(cap[2])
    a = l * w
    b = w * h
    c = l * h
    m = min(a, b, c)
    total += 2 * (a + b + c) + m

    m = max(l, w, h)
    ribbon += 2 * (l + w + h - m) + l * w * h

# --- Results ---
print(f"1. Total square feet of wrapping paper: {total:,d}")
print(f"2. Total feet of ribbon: {ribbon:,d}")
