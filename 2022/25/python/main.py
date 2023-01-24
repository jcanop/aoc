import snafu

# --- Constants ---
INPUT_FILE = "../input/input.txt"

# --- Read and parse the input file ---
with open(INPUT_FILE, "r") as file:
    text = file.read()

total = sum(map(lambda line: snafu.from_snafu(line), text.strip().split("\n")))
snafu = snafu.to_snafu(total)
print(f"SNAFU number to supply to Bob's console: {snafu}")
