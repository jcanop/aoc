from enum import Enum

# --- Constants ---
INPUT_FILE = "../input/input.txt"

# --- Enums ----
class GameResult(Enum):
    LOSE = 0
    DRAW = 3
    WIN = 6

    @staticmethod
    def parse(value):
        if value == "X":
            return GameResult.LOSE
        elif value == "Y":
            return GameResult.DRAW
        elif value == "Z":
            return GameResult.WIN
        else:
            return None

class Shape(Enum):
    ROCK = 1
    PAPER = 2
    SCISSORS = 3

    @staticmethod
    def parse(value):
        if value == "A" or value == "X":
            return Shape.ROCK
        elif value == "B" or value == "Y":
            return Shape.PAPER
        elif value == "C" or value == "Z":
            return Shape.SCISSORS
        else:
            return None

# --- Plays a game ---
def play(p1, p2):
    if p1 == Shape.ROCK:
        if p2 == Shape.ROCK:
            return GameResult.DRAW
        elif p2 == Shape.PAPER:
            return GameResult.LOSE
        elif p2 == Shape.SCISSORS:
            return GameResult.WIN
    elif p1 == Shape.PAPER:
        if p2 == Shape.ROCK:
            return GameResult.WIN
        elif p2 == Shape.PAPER:
            return GameResult.DRAW
        elif p2 == Shape.SCISSORS:
            return GameResult.LOSE
    elif p1 == Shape.SCISSORS:
        if p2 == Shape.ROCK:
            return GameResult.LOSE
        elif p2 == Shape.PAPER:
            return GameResult.WIN
        elif p2 == Shape.SCISSORS:
            return GameResult.DRAW
    else:
        None

# --- Selects what to play ---
def choose(p, result):
    if p == Shape.ROCK:
        if result == GameResult.WIN:
            return Shape.PAPER
        elif result == GameResult.DRAW:
            return Shape.ROCK
        elif result == GameResult.LOSE:
            return Shape.SCISSORS
    elif p == Shape.PAPER:
        if result == GameResult.WIN:
            return Shape.SCISSORS
        elif result == GameResult.DRAW:
            return Shape.PAPER
        elif result == GameResult.LOSE:
            return Shape.ROCK
    elif p == Shape.SCISSORS:
        if result == GameResult.WIN:
            return Shape.ROCK
        elif result == GameResult.DRAW:
            return Shape.SCISSORS
        elif result == GameResult.LOSE:
            return Shape.PAPER

# --- Read the input file ---
total_1 = 0
total_2 = 0
file = open(INPUT_FILE)
while True:
    line = file.readline()
    if not line:
        break

    line = line.strip()

    # --- Puzzle 1 ---
    elf = Shape.parse(line[0])
    me = Shape.parse(line[2])
    result = play(me, elf)
    total_1 += result.value + me.value

    # --- Puzzle 2 ---
    result = GameResult.parse(line[2])
    me = choose(elf, result)
    total_2 += result.value + me.value


print(f"1. Total score: {total_1:,d}")
print(f"2. Total score: {total_2:,d}")
