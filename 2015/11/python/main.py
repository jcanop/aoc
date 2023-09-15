# --- Constants --
INPUT_FILE = "../input/input.txt"

# --- Functions ---
def sanitize(a):
    for i in range(0, len(a)):
        if a[i] == 'i' or a[i] == 'o' or a[i] == 'l':
            a[i] += 1
            for j in range(i + 1, len(a)): a[j] = 'a'
            break

def next_password(a):
    for i in range(len(a) - 1, -1, -1):
        a[i] = chr(ord(a[i]) + 1)
        if a[i] == 'i' or a[i] == 'o' or a[i] == 'l': a[i] = chr(ord(a[i]) + 1)
        if a[i] > 'z': a[i] = 'a'
        else: break

def rule_1(a):
    for i in range(0, len(a) - 2):
        if ord(a[i + 1]) == ord(a[i]) + 1 and ord(a[i + 2]) == ord(a[i]) + 2: return True
    return False

def rule_2(a):
    c = ' '
    n = 0
    for i in range(0, len(a) - 1):
        if a[i] == a[i + 1] and c != a[i]:
            n += 1
            c = a[i]
        if n == 2: return True
    return False

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
text = file.read().strip()
password = [*text]
file.close()

# --- Search next pasword ---
count = 1
sanitize(password)
while True:
    next_password(password)
    if rule_1(password) and rule_2(password):
        print(f"{count}. Next password: {''.join(password)}")
        count += 1
        if count > 2: break
