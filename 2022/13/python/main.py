from functools import total_ordering

# --- Constants --
INPUT_FILE = "../input/input.txt"

# --- Tokenizer ---
class Tokenizer:
    def __init__(self, value):
        self.value = value[1:len(value) - 1]
        self.start = 0
        self.end = 0
        self.count = 0

    def _create_token(self, skip):
        s = self.value[self.start:self.end - skip]
        self.start = self.end
        return s

    def __iter__(self):
        return self

    def __next__(self):
        while self.end < len(self.value):
            c = self.value[self.end]
            self.end += 1

            if   c == "[":
                self.count += 1
            elif c == "]":
                if self.count == 0: return self._create_token(0)
                self.count -= 1
            elif c == ",":
                if self.count == 0: return self._create_token(1)

        if self.end - self.start == 0: raise StopIteration
        return self._create_token(0)

# --- ArrayList ---
@total_ordering
class ArrayList(list):
    def _cmp(self, other):
        _len = max(len(self), len(other))
        for i in range(_len):
            if i == len(self):  return -1
            if i == len(other): return  1
            a = self[i]
            b = other[i]
            if isinstance(a, int) and isinstance(b, int):
                if a != b: return a - b
            else:
                if isinstance(a, int): a = parse_line("[" + str(a) + "]")
                if isinstance(b, int): b = parse_line("[" + str(b) + "]")
                r = a._cmp(b)
                if r != 0: return r
        return 0

    def __eq__(self, other): return self._cmp(other) ==  0
    def __lt__(self, other): return self._cmp(other) < 0
    def __gt__(self, other): return self._cmp(other) > 0


# --- Parse a line from the file ---
def parse_line(line):
    if line.startswith("["):
        ls = ArrayList()
        for token in Tokenizer(line):
            t = parse_line(token)
            if t is not None: ls.append(t)
        return ls
    if line == "":
        return None
    return int(line)

# --- Variables ---
ls = []
total = 0
index = 0

# --- Read and parse the input file ---
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    _id = index // 3 + 1
    if index % 3 == 0:
        ls.append(parse_line(line))
    elif index % 3 == 1:
        ls.append(parse_line(line))
        a = ls[len(ls) - 2]
        b = ls[len(ls) - 1]
        if a < b: total += _id
    elif index % 3 == 2:
        pass
    else:
        raise Exception("Unreachable!")

    index += 1

# --- Puzzle 1 ---
print(f"1. The sum of the indices of the right order pairs: {total:,d}")

# --- Puzzle 2 ---
divider_1 = parse_line("[[2]]")
divider_2 = parse_line("[[6]]")
ls.append(divider_1)
ls.append(divider_2)
ls.sort()
index_1 = ls.index(divider_1) + 1
index_2 = ls.index(divider_2) + 1
total = index_1 * index_2
print(f"2. The decoder key is: {total:,d}")
