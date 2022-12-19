public class ShapeFactory {
	// --- Constants ---
	private static final int COUNT = 5;
	// ####
	private static final int[][] SHAPE_1 = new int[][] { new int[] { 0, 1, 2, 3 } };
	// .#.
	// ###
	// .#.
	private static final int[][] SHAPE_2 =  new int[][] { new int[] { 1 }, new int[] { 0, 1, 2 }, new int[] { 1 } };
	// ..#
	// ..#
	// ###
	private static final int[][] SHAPE_3 =  new int[][] { new int[] { 2 }, new int[] { 2 }, new int[] { 0, 1, 2 } };
	// #
	// #
	// #
	// #
	private static final int[][] SHAPE_4 =  new int[][] { new int[] { 0 }, new int[] { 0 }, new int[] { 0 }, new int[] { 0 } };
	// ##
	// ##
	private static final int[][] SHAPE_5 = new int[][] { new int[] { 0, 1 }, new int[] { 0, 1 } };
	
	private int index;

	public ShapeFactory() {
		this.index = 0;
	}

	public int getCount() {
		return COUNT;
	}

	public int[][] next() {
		switch (index++ % COUNT) {
		case 0: return SHAPE_1;
		case 1: return SHAPE_2;
		case 2: return SHAPE_3;
		case 3: return SHAPE_4;
		case 4: return SHAPE_5;
		default: throw new RuntimeException("Invalid index value: " + index);
		}
	}
}
