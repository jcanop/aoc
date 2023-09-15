# --- Constants --
INPUT_FILE = "../input/input.txt"
LIST_1 = ['a', 'e', 'i', 'o', 'u']
LIST_2 = ["ab", "cd", "pq", "xy"]

# --- Functions ---
def rule_1_1(s):
    ls = [*s]
    count = 0
    for c in ls:
        try:
            LIST_1.index(c)
            count += 1
            if count == 3: return True
        except ValueError:
            pass
    return False

def rule_1_2(s):
    ls = [*s]
    for i in range(0, len(ls) - 1):
        if ls[i] == ls[i + 1]: return True
    return False

def rule_1_3(s):
    for c in LIST_2:
        if s.find(c) != -1: return False
    return True

def rule_2_1(s):
    ls = [*s]
    for i in range(0, len(ls) - 2):
        for j in range(i + 2, len(ls) - 1):
            if ls[i] == ls[j] and ls[i + 1] == ls[j + 1]: return True
    return False

def rule_2_2(s):
    ls = [*s]
    for i in range(0, len(ls) - 2):
            if ls[i] == ls[i + 2]: return True
    return False

# --- Variables ---
count1 = 0
count2 = 0

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    # --- Puzzle 1 ---
    if rule_1_1(line) and rule_1_2(line) and rule_1_3(line): count1 += 1

    # --- Puzzle 1 ---
    if rule_2_1(line) and rule_2_2(line): count2 += 1

file.close()

# --- Results ---
print(f"1. Nice Strings: {count1:,d}")
print(f"2. Nice Strings: {count2:,d}")
