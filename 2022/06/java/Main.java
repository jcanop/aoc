import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";
	private static final int C_PACKET = 4;
	private static final int C_MESSAGE = 14;

	/**
	 * Indicates if an array has unique characters.
	 *
	 * @param data An array of characters to search.
	 * @param offset Start of the data to proccess.
	 * @param len Length of data to proccess..
	 * @return true if all characters are unique, otherwise false.
	 */
	private static boolean unique(char[] data, int offset, int len) {
		for (int i = offset; i < offset + len; i++)
			for (int j = offset; j < offset + len; j++)
				if (i != j && data[i] == data[j])
					return false;
		return true;
	}

	/**
	 * Search the stream of data for a marker.
	 *
	 * @param data An array of characters to search.
	 * @param c Constant of the marker.
	 * @return The index of the marker or -1 if not marker was found.
	 */
	private static int find(char[] data, int c) {
		int len = data.length;
		if (len < c) return - 1;
		for (int i = 0; i < len - c; i++)
			if (unique(data, i, c))
				return i + c;

		return -1;
	}

	/**
	 * Search for the start of a packet.
	 *
	 * @param data An array of characters to search.
	 */
	public static int findPacket(char[] data) {
		return find(data, C_PACKET);
	}

	/**
	 * Search for the sstart of a message.
	 *
	 * @param data An array of characters to search.
	 */
	public static int findMessage(char[] data) {
		return find(data, C_MESSAGE);
	}

	public static void main(String[] args) throws IOException {
		byte[] bytes = Files.readAllBytes(Paths.get(INPUT_FILE));
		char[] chars = new String(bytes).toCharArray();

		System.out.format("Part 1. Start-of-packet marker: %,d%n", findPacket(chars));
		System.out.format("Part 2. Start-of-message marker: %,d%n", findMessage(chars));
	}
}
