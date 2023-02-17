import re

# --- Constants ---
ORE = 0
CLAY = 1
OBSIDIAN = 2
GEODE = 3
M = 4
MASK = 8
TIME = 9

REGEX = r"Blueprint (\d+):\s+Each ore robot costs (\d+) ore.\s+Each clay robot costs (\d+) ore.\s+Each obsidian robot costs (\d+) ore and (\d+) clay.\s+Each geode robot costs (\d+) ore and (\d+) obsidian."

# --- Blueprint ---
class Blueprint:
    def __init__(self, cap):
        self.id = int(cap[0])
        self.costs = [ [0] * 3 for _ in range(4) ]
        self.max_costs = [0] * 3;
        self.costs[ORE][ORE] = int(cap[1])
        self.costs[CLAY][ORE] = int(cap[2])
        self.costs[OBSIDIAN][ORE] = int(cap[3])
        self.costs[OBSIDIAN][CLAY] = int(cap[4])
        self.costs[GEODE][ORE] = int(cap[5])
        self.costs[GEODE][OBSIDIAN] = int(cap[6])
        for i in range(3):
            for j in range(4):
                   self.max_costs[i] = max(self.max_costs[i], self.costs[j][i])
    @staticmethod
    def load(filename):
        with open(filename) as file:
            text = file.read()
        return [ Blueprint(cap) for cap in re.findall(REGEX, text) ]

# --- State ---
def create_state():
    return [1, 0, 0, 0, 0, 0, 0, 0, 0, 0]

def clone_state(state):
    a = create_state()
    for i in range(len(a)): a[i] = state[i]
    a[MASK] = 0
    return a

def mine(state):
    for i in range(4): state[M + i] += state[i]
    state[TIME] += 1

def should_build(state, bp, robot):
    # --- Check if we have enough materials ---
    for i in range(3):
        if state[M + i] < bp.costs[robot][i]: return False

    # --- Always build a Geode robot if we can ---
    if robot == GEODE: return True

    # --- Check for bit mask ---
    m = (1 << robot)
    if (state[MASK] & m) == m: return False

    # --- We shouldn't build more robots than needed ---
    return state[robot] < bp.max_costs[robot]

def build(state, bp, robot):
    for i in range(3): state[M + i] -= bp.costs[robot][i]
    state[robot] += 1

# --- Find the optimal path for a blueprint ---
def get_max(bp, limit):
    _max = 0
    queue = [ create_state() ]

    while len(queue) > 0:
        state = queue.pop(0)

        if state[TIME] == limit:
            _max = max(_max, state[M + GEODE])
            continue

        if should_build(state, bp, GEODE):
            mine(state)
            build(state, bp, GEODE)
            queue.append(state)
            continue

        for i in reversed(range(3)):
            if should_build(state, bp, i):
                clone = clone_state(state)
                mine(clone)
                build(clone, bp, i)
                queue.append(clone)
                state[MASK] |= (1 << i)

        mine(state)
        queue.append(state)

    return _max
