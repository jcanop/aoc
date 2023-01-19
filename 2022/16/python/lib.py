import re

# --- Constants ---
REGEX = r"Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z]{2}(?:,\s*[A-Z]{2})*)"

# --- Floyd Warshall Algorithm ---
def floyd_warshall(valves, tunnels):
    distances = dict()
    for _from in valves:
        m = dict()
        for to in valves:
            m[to] = len(valves) + 1
        distances[_from] = m

    for (_from, ls) in tunnels.items():
        for to in ls:
            distances[_from][to] = 1

    for _from in valves:
        distances[_from][_from] = 0

    for via in valves:
        for _from in valves:
            for to in valves:
                d = distances[_from][via] + distances[via][to]
                if distances[_from][to] > d:
                    distances[_from][to] = d

    return distances

# --- Cave Layout ---
class Layout:
    def __init__(self, filename):
        self.flows = dict()
        self.useful_valves = set()
        self.all = None

        valves = set()
        tunnels = dict()

        with open(filename, "r") as file:
            text = file.read().strip()

        for cap in re.findall(REGEX, text):
            _id = cap[0]
            flow = int(cap[1])
            routes = cap[2].split(", ")

            self.flows[_id] = flow
            valves.add(_id)
            tunnels[_id] = routes
            if flow > 0: self.useful_valves.add(_id)

        self.all = floyd_warshall(valves, tunnels)

    # --- Gets the sum of the flow of the open valves ---
    def get_flow(self, _set):
        return sum([ self.flows[x] for x in _set ])

    # --- Depth First Search: Puzzle 1 ---
    def dfs1(self):
       _open = set()
       return self._dfs1("AA", 0, 0, _open)

    def _dfs1(self, current, time, total, _open):
        _max = total + self.get_flow(_open) * (30 - time)
        for _next in self.useful_valves:
            if _next in _open: continue
            delta = self.all[current][_next] + 1
            if time + delta >= 30: continue
            new_total = total + delta * self.get_flow(_open)
            _open.add(_next)
            value = self._dfs1(_next, time + delta, new_total, _open)
            _max = max(_max, value)
            _open.remove(_next)
        return _max

    # --- Depth First Search: Puzzle 2 ---
    def dfs2(self):
        _open = set()
        useful = set(self.useful_valves)
        return self._dfs2("AA", False, 0, 0, _open, useful)

    def _dfs2(self, current, elephant, time, total, _open, useful):
        _max = total + self.get_flow(_open) * (26 - time)
        if elephant == False:
            new_candidates = set(useful)
            for  v in _open: new_candidates.remove(v)
            new_open = set()
            max_elephant = self._dfs2("AA", True, 0, 0, new_open, new_candidates)
            _max = total + self.get_flow(_open) * (26 - time) + max_elephant
        for _next in useful:
            if _next in _open: continue
            delta = self.all[current][_next] + 1
            if time + delta >= 26: continue
            new_total = total + delta * self.get_flow(_open)
            _open.add(_next)
            value = self._dfs2(_next, elephant, time + delta, new_total, _open, useful)
            _max = max(_max, value)
            _open.remove(_next)
        return _max

