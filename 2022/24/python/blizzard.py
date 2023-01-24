# --- Constants ---
NORTH = '^'
SOUTH = 'v'
WEST = '<'
EAST = '>'
NONE = '.'

# --- Blizzards going east and west ---
class Row:
    def __init__(self):
        self.east = []
        self.west = []

    def add(self, d):
        if   d == EAST: self.east.append(True);  self.west.append(False)
        elif d == WEST: self.east.append(False); self.west.append(True)
        else:           self.east.append(False); self.west.append(False)

    def is_empty(self, x, time):
        ln = len(self.east)
        wi = (x - 1 + time) % ln
        ei = (x - 1 + ln - (time % ln)) % ln
        return not self.east[ei] and not self.west[wi]

# --- Blizzards going north and south ---
class Col:
    def __init__(self):
        self.north = []
        self.south = []

    def add(self, d):
        if   d == NORTH: self.north.append(True);  self.south.append(False)
        elif d == SOUTH: self.north.append(False); self.south.append(True)
        else:            self.north.append(False); self.south.append(False)

    def is_empty(self, y, time):
        ln = len(self.north)
        ni = (y - 1 + time) % ln
        si = (y - 1 + ln - (time % ln)) % ln
        return not self.north[ni] and not self.south[si]

# --- Blizzard ---
class Blizzard:
    def __init__(self):
        self.cols = []
        self.rows = []

    def add(self, line):
        row = Row()
        while len(self.cols) < len(line): self.cols.append(Col())
        for x, d in enumerate(line):
            self.cols[x].add(d)
            row.add(d)
        self.rows.append(row)

    def is_empty(self, x, y, time):
        return self.cols[x - 1].is_empty(y, time) and self.rows[y - 1].is_empty(x, time)
