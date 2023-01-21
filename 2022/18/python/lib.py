class Point:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    def derive(self, dx, dy, dz):
        x = self.x + dx
        y = self.y + dy
        z = self.z + dz
        return Point(x, y, z)

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y and self.z == other.z

    def __hash__(self):
        return 7 * self.x + 19 * self.y + 31 * self.z

    def __str__(self):
        return self.x + "," + self.y + "," + self.z
