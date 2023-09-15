import re

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"Enter the code at row (\d+), column (\d+)"

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
text = file.read().strip()
file.close()

cap = re.search(REGEX, text)
row = int(cap[1])
column = int(cap[2])

# --- Find the code ---
n = row + column - 1
target = int(((pow(n, 2) + n) / 2) - (n - column))
code = 20151125
for _ in range(1, target): code = (code * 252533) % 33554393
print(f"Code: {code:,d}")
