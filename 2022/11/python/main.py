import re
import copy
from functools import reduce

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"Monkey (\d+):\s*Starting items: ([\d, ]+)\s*Operation: new = old ([\*\+]) (\d+|old)\s*Test: divisible by (\d+)\s*If true: throw to monkey (\d+)\s*If false: throw to monkey (\d+)"

# --- Monkey ---
class Monkey:
    def __init__(self, mid, items, operator, operand, divisible, throw_true, throw_false):
        self.id = mid
        self.items = items
        self.operator = operator
        self.operand = operand
        self.divisible = divisible
        self.throw_true = throw_true
        self.throw_false = throw_false
        self.inspects = 0

    def play(self, group, relief, gcd = 0):
        while len(self.items) > 0:
            self.inspects += 1
            item = self.items.pop(0)
            value = item
            if self.operand is not None: value = self.operand

            if    self.operator == "+": item += value
            elif  self.operator == "*": item *= value
            else: raise Exception("Unsupported operator: " + self.operand)

            if    relief == 1: item //=3
            elif  relief == 2: item %= gcd
            else: raise Excpetion("Unsupported relief: " + relief)

            index = self.throw_false
            if item % self.divisible == 0: index = self.throw_true
            group[index].items.append(item)

# --- Calcualte Monkey Business ---
def calculate_monkey_business(group):
    ls = sorted(list(map(lambda m: m.inspects, group)), reverse=True)
    return ls[0] * ls[1]

# --- Read and parse the input file --
group_1 = []
group_2 = []
file = open(INPUT_FILE, "r")
text = file.read().strip()
for cap in re.findall(REGEX, text):
    mid = int(cap[0])
    items = list(map(lambda x: int(x), cap[1].split(", ")))
    operator = cap[2]
    operand = None
    if cap[3] != "old": operand = int(cap[3])
    divisible = int(cap[4])
    throw_true = int(cap[5])
    throw_false = int(cap[6])
    monkey = Monkey(mid, items, operator, operand, divisible, throw_true, throw_false)
    group_1.append(copy.deepcopy(monkey))
    group_2.append(monkey)

# --- Puzzle 1 ---
for _ in range(20):
    for m in group_1: m.play(group_1, 1)
total = calculate_monkey_business(group_1)
print(f"1. Level of monkey business: {total:,d}")

# --- Puzzle 2 ---
gcd = reduce(lambda acc, val: acc * val, [ m.divisible for m in group_2 ], 1)
for _ in range(10000):
    for m in group_2: m.play(group_2, 2, gcd)
total = calculate_monkey_business(group_2)
print(f"2. Level of monkey business: {total:,d}")

