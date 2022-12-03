INPUT_FILE = "../input/input.txt"

list = []
total = 0

# --- Read and parse the input file ---
file = open(INPUT_FILE)
while True:
    line = file.readline()
    if not line:
        break

    line = line.strip()
    if len(line) == 0:
        list.append(total)
        total = 0
    else:
        total += int(line)
file.close()
list.append(total)
list.sort(reverse = True)

# --- Find the Elf carrying the most Calories ---
max = list[0]
print(f"The Elf carrying the most Calories, is carrying {max:,d} Calories.")

# --- Find the top three Elves carrying the most Calories ---
total = sum(list[0:3])
print(f"The top 3 Elves carrying the most Calores, are carrying {total:,d} Calories.")
