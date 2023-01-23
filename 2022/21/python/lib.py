import re

# --- Constants ---
RE_KEY = r"([a-z]{4})"
RE_OP = r"([+\-*/])"
RE_QUEUE = RE_KEY + ": " + RE_KEY + r"\s+" + RE_OP + r"\s+" + RE_KEY
RE_SOLVED = RE_KEY + r": (\d+)"

# --- Operation ---
class Operation:
    def __init__(self, key, op1, op2, operator):
        self.key = key
        self.op1 = op1
        self.op2 = op2
        self.operator = operator

    @staticmethod
    def parse(cap):
        key = cap[0]
        op1 = cap[1]
        op2 = cap[3]
        operator = cap[2]
        return Operation(key, op1, op2, operator)

    def __str__(self):
        return self.key + ": " + self.op1 + " " + self.operator + " " + self.op2

    def solve_for_variable(self, i):
        if i == 1:
            if   self.operator == "+": self.operator = "-"
            elif self.operator == "-": self.operator = "+"
            elif self.operator == "*": self.operator = "/"
            elif self.operator == "/": self.operator = "*"
            else: raise Exception("Unsupported operator: " + self.operator)
            self.key, self.op1 = self.op1, self.key
        elif i == 2:
            swap = False
            if   self.operator == "+": self.operator = "-"; swap = True
            elif self.operator == "-": pass
            elif self.operator == "*": self.operator = "/"; swap = True
            elif self.operator == "/": pass
            else: raise Exception("Unsupported operator: " + self.operator)
            self.key, self.op2 = self.op2, self.key
            if swap: self.op1, self.op2 = self.op2, self.op1
        else: raise Exception("Unsupported argument: " + i)

# --- Resolver ---
class Resolver:
    def __init__(self, filename):
        with open(filename) as file:
            text = file.read().strip()

        self.queue = [ Operation.parse(cap) for cap in re.findall(RE_QUEUE, text) ]
        self.solved = dict([ (cap[0], int(cap[1])) for cap in re.findall(RE_SOLVED, text) ])

    def update_for_puzzle2(self, root, me):
        for op in self.queue:
            if op.key == root: op.operator = "="
            if   op.op1 == me: op.solve_for_variable(1)
            elif op.op2 == me: op.solve_for_variable(2)
        self.solved.pop(me)

    def solve_for(self, _id):
        while len(self.queue):
            op = self.queue.pop()
            v1 = self.solved.get(op.op1)
            v2 = self.solved.get(op.op2)

            if op.operator == "=":
                if v1 is not None  and v2 is None:
                    self.add_solution(op.op2, v1)
                elif v1 is None and v2 is not None:
                    self.add_solution(op.op1, v2)

            if v1 is not None and v2 is not None:
                result = None
                if   op.operator == "+": result = v1 + v2
                elif op.operator == "-": result = v1 - v2
                elif op.operator == "*": result = v1 * v2
                elif op.operator == "/": result = v1 // v2
                elif op.operator == "=": pass
                else: raise Exception("Unsupported operator: " + op.operator)
                if op.operator != "=":
                    self.add_solution(op.key, result)
            else:
                self.queue.insert(0, op)
        return self.solved[_id]

    def add_solution(self, key, value):
        self.solved[key] = value
        for op in filter(lambda x: x.key == key, self.queue):
            v1 = self.solved.get(op.op1)
            v2 = self.solved.get(op.op2)
            if   v1 is not None and v2 is None: op.solve_for_variable(2)
            elif v1 is None and v2 is not None: op.solve_for_variable(1)
