from lib import Chamber, SHAPES

# --- Constants ---
INPUT_FILE = "../input/input.txt"
LIMIT_1 = 2022
LIMIT_2 = 1000000000000

# --- Runs the simulations ---
def sim(flows, limit):
    count = 0
    rocks = 1
    gcd = len(SHAPES) * len(flows)
    chamber = Chamber()
    chamber.add_shape(SHAPES[0])

    # --- Variables for tracking a pattern ---
    prev_height = 0
    prev_rocks = 0
    delta_height = 0
    delta_rocks = 0
    simulated = 0

    # --- Iterate until the limit is reached ---
    while True:
        if count > 0 and count % gcd == 0:
            height = chamber.height()
            dh = height - prev_height
            dr = rocks - prev_rocks

            # --- Pattern found ---
            if dh == delta_height and dr == delta_rocks:
                rate = (limit - rocks) // dr
                simulated = dh * rate + 1
                rocks = limit - ((limit - rocks) % dr)
            prev_height = height
            prev_rocks = rocks
            delta_height = dh
            delta_rocks = dr

        # --- Normal simulation ---
        chamber.move(flows[count % len(flows)])
        if chamber.move_down() == False:
            if rocks > limit:
                return chamber.height() + simulated
            chamber.add_shape(SHAPES[rocks % len(SHAPES)])
            rocks += 1
        count += 1

# --- Read and parse the input file ---
with open(INPUT_FILE) as file:
    flows = file.read().strip()

height = sim(flows, LIMIT_1)
print(f"1. The tower is {height:,d} units tall")

height = sim(flows, LIMIT_2)
print(f"2. The tower is {height:,d} units tall")
