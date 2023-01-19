class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __str__(self):
        return str(self.x) + "," + str(self.y)

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        return hash(str(self))

class Sensor:
    def __init__(self, x, y, bx, by):
        self.x = x
        self.y = y
        self.range = abs(x - bx) + abs(y - by)

    def is_in_range(self, x, y):
        return abs(self.x - x) + abs(self.y - y) <= self.range

class Map:
    def __init__(self):
        self.sensors = {}
        self.beacons = []
        self.point = Point(0, 0)

    def add(self, sx, sy, bx, by):
        self.sensors[Point(sx, sy)] = Sensor(sx, sy, bx, by)
        self.beacons.append(Point(bx, by))

    def is_in_sensor_range(self, x, y):
        for p in self.sensors:
            if self.sensors[p].is_in_range(x, y): return True
        return False

    def is_empty(self, x, y):
        self.point.x = x
        self.point.y = y
        return (self.sensors.get(self.point) is None) and (self.point not in self.beacons)
