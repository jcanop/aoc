import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class Map {
	private final HashMap<Point, Sensor> sensors;
	private final List<Point> beacons;
	private Point pointer;

	public Map () {
		this.sensors = new HashMap<>();
		this.beacons = new ArrayList<>();
		this.pointer = new Point(0, 0);
	}

	public void addSensor(int x, int y, int bx, int by) {
		sensors.put(new Point(x, y), new Sensor(x, y, bx, by));
		beacons.add(new Point(bx, by));
	}

	public List<Sensor> getSensors() {
		return new ArrayList<Sensor>(sensors.values());
	}

	public boolean isInSensorRange(int x, int y) {
		for (Sensor s: sensors.values())
			if (s.isInRange(x, y))
				return true;
		return false;
	}

	public boolean isEmpty(int x, int y) {
		pointer.x = x;
		pointer.y = y;
		return !sensors.keySet().contains(pointer) && !beacons.contains(pointer);
	}
}
