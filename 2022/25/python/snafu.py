def to_snafu(value):
    ls = []
    while value > 0:
        overflow = False
        m = value % 5
        if   m == 0: ls.insert(0, "0")
        elif m == 1: ls.insert(0, "1")
        elif m == 2: ls.insert(0, "2")
        elif m == 3: ls.insert(0, "="); overflow = True
        elif m == 4: ls.insert(0, "-"); overflow = True
        value //= 5
        if overflow: value += 1
    return "".join(ls)

def from_snafu(value):
    result = 0
    for i, c in enumerate(value):
        p = 5 ** (len(value) - i - 1)
        if   c == "0": pass
        elif c == "1": result += 1 * p
        elif c == "2": result += 2 * p
        elif c == "=": result -= 2 * p
        elif c == "-": result -= 1 * p
        else: raise Exception("Illegal character: " + c)
    return result

