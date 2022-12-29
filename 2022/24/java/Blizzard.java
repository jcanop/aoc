import java.util.ArrayList;
import java.util.List;

public class Blizzard {
	// --- Constants ---
	private static final char NORTH = '^';
	private static final char SOUTH = 'v';
	private static final char WEST  = '<';
	private static final char EAST  = '>';

	private static class Row {
		private final List<Boolean> east;
		private final List<Boolean> west;

		private Row() {
			east = new ArrayList<>();
			west = new ArrayList<>();
		}

		private void add(char dir) {
			switch (dir) {
				case EAST: east.add(true); west.add(false); break;
				case WEST: east.add(false); west.add(true); break;
				default: east.add(false); west.add(false);
			}
		}

		private boolean isEmpty(int x, int time) {
			int len = east.size();
			int wi = (x - 1 + time) % len;
			int ei = (x - 1 + len - (time % len)) % len;
			return !east.get(ei) && !west.get(wi);
		}
	}

	private static class Col {
		private final List<Boolean> north;
		private final List<Boolean> south;

		private Col() {
			north = new ArrayList<>();
			south = new ArrayList<>();
		}

		private void add(char dir) {
			switch (dir) {
				case NORTH: north.add(true); south.add(false); break;
				case SOUTH: north.add(false); south.add(true); break;
				default: north.add(false); south.add(false);
			}
		}

		private boolean isEmpty(int y, int time) {
			int len = north.size();
			int ni = (y - 1 + time) % len;
			int si = (y - 1 + len - (time % len)) % len;
			return !north.get(ni) && !south.get(si);
		}
	}

	private final List<Col> cols;
	private final List<Row> rows;

	public Blizzard() {
		cols = new ArrayList<>();
		rows = new ArrayList<>();
	}

	public void add(char[] chars) {
		Row row = new Row();
		rows.add(row);
		while (cols.size() < chars.length) cols.add(new Col());
		for (int x = 0; x < chars.length; x++) {
			char d = chars[x];
			Col col = cols.get(x);
			col.add(d);
			row.add(d);
		}
	}

	public boolean isEmpty(int x, int y, int time) {
		return cols.get(x - 1).isEmpty(y, time) && rows.get(y - 1).isEmpty(x, time);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (int y = 0; y < rows.size(); y++) {
			for (int x = 0; x < cols.size(); x++) {
				Col col = cols.get(x);
				Row row = rows.get(y);
				if      (col.north.get(y)) sb.append('^');
				else if (col.south.get(y)) sb.append('v');
				else if (row.west.get(x))  sb.append('<');
				else if (row.east.get(x))  sb.append('>');
				else sb.append('.');
			}
			sb.append('\n');
		}
		return sb.toString();
	}
}
