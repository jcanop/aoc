from blizzard import Blizzard

class Layout:
    def __init__(self, filename):
        self.blizzard = Blizzard()

        with open(filename, "r") as file:
            lines = file.read().strip().split("\n")

        self.height = len(lines)
        self.width = len(lines[0])
        for y in range(1, self.height - 1):
            self.blizzard.add(lines[y][1:-1])
        self.data = [ [' '] * self.width for _ in range(self.height) ]
        self.next = [ [' '] * self.width for _ in range(self.height) ]

    # --- Checks if you can move to a space at a time ---
    def can_move(self, x, y, time):
        if x == 1 and y == 0: return True # Start position
        if x == self.width - 2 and y == self.height - 1: return True # End position
        if x < 1 or x >= self.width - 1 or y < 1 or y >= self.height - 1: return False # Border
        return self.blizzard.is_empty(x, y, time)

    # --- Search the fastest path between two points ---
    def search(self, start, end, time):
        for y in range(self.height):
            for x in range(self.width):
                self.data[y][x] = ' '
        self.data[start[1]][start[0]] = 'X'

        while self.data[end[1]][end[0]] != 'X':
            for y in range(self.height):
                for x in range(self.width):
                    self.next[y][x] = ' '
            for y in range(self.height):
                for x in range(self.width):
                    if self.data[y][x] == 'X':
                        if                         self.can_move(x    , y, time + 1): self.next[y][x]     = 'X'
                        if x > 0               and self.can_move(x - 1, y, time + 1): self.next[y][x - 1] = 'X'
                        if x < self.width - 1  and self.can_move(x + 1, y, time + 1): self.next[y][x + 1] = 'X'
                        if y > 0               and self.can_move(x, y - 1, time + 1): self.next[y - 1][x] = 'X'
                        if y < self.height - 1 and self.can_move(x, y + 1, time + 1): self.next[y + 1][x] = 'X'
            time += 1
            for y in range(self.height):
                for x in range(self.width):
                    self.data[y][x] = self.next[y][x]

        return time
