import hashlib

# --- Constants --
INPUT_FILE = "../input/input.txt"

# --- Variables ---
found1 = False
found2 = False
count = 0

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
secret = file.read().strip()

while True:
    text = secret + str(count)
    digest = hashlib.md5(text.encode()).hexdigest()

    # --- Puzzle 1 ---
    if not found1 and digest[0:5] == "00000":
        print(f"1. First hash with five zeros: {count:,d}")
        found1 = True

    # --- Puzzle 2 ---
    if not found2 and digest[0:6] == "000000":
        print(f"2. First hash with six zeros: {count:,d}")
        found2 = True

    if found1 and found2: break
    count += 1
