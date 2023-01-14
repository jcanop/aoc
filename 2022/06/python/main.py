# --- Constants ---
INPUT_FILE = "../input/input.txt"
C_PACKET = 4
C_MESSAGE = 14

# --- Indicates if an array has unique characters ---
def unique(data):
    ln = len(data)
    for i in range(ln):
        for j in range(ln):
            if i != j and data[i] == data[j]: return False
    return True

# --- Search the stream of data for a marker ---
def find(data, c):
    ln = len(data)
    if ln < c: return None
    for i in range(ln - c):
        if unique(data[i:i+c]):
            return i + c
    return None

# --- Search for the start of a packet ---
def find_packet(data):
    return find(data, C_PACKET)

# --- Search for the start of a message ---
def find_message(data):
    return find(data, C_MESSAGE)

# --- Read the input file ---
file = open(INPUT_FILE, "r")
data = file.read().strip()

# --- Puzzle 1 ---
start = find_packet(data)
print(f"1. Start-of-packet marker: {start:,d}")

# --- Puzzle 2 ---
start = find_message(data)
print(f"2. Start-of-message marker: {start:,d}")
