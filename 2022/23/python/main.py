from lib import Grove

# --- Constants ---
INPUT_FILE = "../input/input.txt"

# --- Read and parse the input file ---
grove = Grove(INPUT_FILE)
while grove.sim():
    if grove.round == 10:
        total = grove.empty_count()
        print(f"1. Empty ground tiles: {total:,d}")
print(f"2. First round with no Elf moves: {grove.round:,d}")

