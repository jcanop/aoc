import re

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"([a-z]+): (\d+)"

# --- MFCSAM Output ---
MAP = {
    'children': 3,
    'cats': 7,
    'samoyeds': 2,
    'pomeranians': 3,
    'akitas': 0,
    'vizslas': 0,
    'goldfish': 5,
    'trees': 3,
    'cars': 2,
    'perfumes': 1
}

# --- Functions ---
def find1(line):
    for cap in re.findall(REGEX, line):
        key = cap[0]
        value = int(cap[1])
        if key in MAP and MAP[key] != value: return False
    return True

def find2(line):
    for cap in re.findall(REGEX, line):
        key = cap[0]
        value = int(cap[1])
        if key in MAP:
            if key == "cats" or key == "trees":
                if MAP[key] >= value: return False
            elif key == "pomeranians" or key == "goldfish":
                if MAP[key] <= value: return False
            else:
                if MAP[key] != value: return False
    return True

# --- Variables ---
sue1 = ""
sue2 = ""

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    if sue1 == "" and find1(line): sue1 = line.split(":")[0]
    if sue2 == "" and find2(line): sue2 = line.split(":")[0]
file.close()

# --- Results ---
print(f"1. Number of the Sue that got you the gift: {sue1}")
print(f"2. Number of the Sue that got you the gift: {sue2}")
