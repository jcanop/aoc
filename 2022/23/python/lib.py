# --- Point ---
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        return 7 * self.x + 31 * self.y

    def __str__(self):
        return "(" + str(self.x) + "," + str(self.y) + ")"

    def derive(self, dx, dy):
        return Point(self.x + dx, self.y + dy)

# --- Grove ---
class Grove:
    def __init__(self, filename):
        self.elves = set()
        self.proposed = dict()
        self.banned = set()
        self.round = 0

        with open(filename) as file:
            text = file.read().strip()

        for y, line in enumerate(text.split("\n")):
            for x, c in enumerate(line):
                if c == "#": self.elves.add(Point(x, y))

    # --- Simulates one round ---
    def sim(self):
        result = False

        # --- Phase 1: Considers move ---
        for pos in self.elves:
            found = False
            for y in range(-1, 2):
                for x in range(-1, 2):
                    if x == 0 and y == 0: continue
                    p = pos.derive(x, y)
                    if p in self.elves: found = True; break
                if found: break

            # --- No Elf adjacent, then do nothing ---
            if not found:
                self.proposed[pos] = pos
                continue

            # --- Look at the 4 directions ---
            p = None
            for n in range(4):
                i = (n + self.round) % 4
                if i == 0: # North
                    if  pos.derive(-1, -1) in self.elves or \
                        pos.derive( 0, -1) in self.elves or \
                        pos.derive( 1, -1) in self.elves: continue
                    p = pos.derive( 0, -1)
                    break
                elif i == 1: # South
                    if  pos.derive(-1,  1) in self.elves or \
                        pos.derive( 0,  1) in self.elves or \
                        pos.derive( 1,  1) in self.elves: continue
                    p = pos.derive( 0,  1)
                    break
                elif i == 2: # West
                    if  pos.derive(-1, -1) in self.elves or \
                        pos.derive(-1,  0) in self.elves or \
                        pos.derive(-1,  1) in self.elves: continue
                    p = pos.derive(-1,  0)
                    break
                elif i == 3: # East
                    if  pos.derive( 1, -1) in self.elves or \
                        pos.derive( 1,  0) in self.elves or \
                        pos.derive( 1,  1) in self.elves: continue
                    p = pos.derive( 1,  0)
                    break
                else: raise Exception("Unreacheable!")

            if p is not None:
                if   p in self.banned: self.proposed[pos] = pos
                elif p in self.proposed:
                    self.banned.add(p)
                    e = self.proposed.pop(p)
                    self.proposed[e] = e
                    self.proposed[pos] = pos
                else:
                    self.proposed[p] = pos
                result = True
            else:
                self.proposed[pos] = pos

        # --- Phase 2: Move ---
        self.elves.clear()
        for pos in self.proposed:
            self.elves.add(pos)
        self.proposed.clear()
        self.banned.clear()
        self.round += 1

        return result

    def _find_view_rect(self):
        minx = min([p.x for p in self.elves])
        miny = min([p.y for p in self.elves])
        maxx = max([p.x for p in self.elves])
        maxy = max([p.y for p in self.elves])
        return (minx, miny, maxx, maxy)

    def empty_count(self):
        total = 0
        (minx, miny, maxx, maxy) = self._find_view_rect()
        for y in range(miny, maxy + 1):
            for x in range(minx, maxx + 1):
                if Point(x, y) not in self.elves: total += 1
        return total

    def print(self):
        (minx, miny, maxx, maxy) = self._find_view_rect()
        for y in range(miny, maxy + 1):
            for x in range(minx, maxx + 1):
                if Point(x, y) in self.elves: print("#", end="")
                else: print(".", end="")
            print("")
