public class GridMap extends Map {
	public GridMap(String data) {
		super(data);
	}

	@Override
	protected void move(int steps) {
		int x = pointer.x;
		int y = pointer.y;
		while (steps > 0) {
			switch (direction) {
				case NORTH: y--; if (y < 0) y = height - 1;  break;
				case SOUTH: y++; if (y == height - 1) y = 0; break;
				case WEST: x--;  if (x < 0) x = width -1 ;   break;
				case EAST: x++;  if (x == width - 1) x = 0;  break;
				default: throw new IllegalArgumentException("" +direction);
			}
			if (grid[y][x] == WALL) return;
			if (grid[y][x] == OPEN) {
				steps--;
				pointer.x = x;
				pointer.y = y;
				path[y][x] = direction;
			}
		}
	}
}
