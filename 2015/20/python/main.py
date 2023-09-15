import collections
import itertools

# --- Constants --
INPUT_FILE = "../input/input.txt"

# --- Functions ---
# Divisors unctions based on:
#   https://alexwlchan.net/2019/finding-divisors-with-python/
def prime_factors(n):
    i = 2
    while i * i <= n:
        if n % i == 0:
            n /= i
            yield i
        else:
            i += 1
    if n > 1:
        yield n

def prod(iterable):
    result = 1
    for i in iterable:
        result *= i
    return result

def get_divisors(n):
    pf = prime_factors(n)
    pf_with_multiplicity = collections.Counter(pf)
    powers = [
        [factor ** i for i in range(count + 1)]
        for factor, count in pf_with_multiplicity.items()
    ]
    for prime_power_combo in itertools.product(*powers):
        yield prod(prime_power_combo)

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
text = file.read().strip()
_input = int(text)
file.close()

# --- Puzzle 1 ---
house = 0
presents = 0
while presents < _input:
    house += 1
    presents = 10 * sum(get_divisors(house))
print(f"1. House number: {house:,d}")

# --- Puzzle 2 ---
house = 0
presents = 0
while presents < _input:
    house += 1
    presents = 11 * sum(filter(lambda x: x * 50 > house, get_divisors(house)))
print(f"2. House number: {house:,d}")
