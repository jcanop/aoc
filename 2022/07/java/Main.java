import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Main {
	// --- Constants ---
	private static final String INPUT_FILE = "../input/input.txt";
	private static final int LIMIT = 100_000;
	private static final int DISK_SIZE = 70_000_000;
	private static final int UPDATE_SIZE = 30_000_000;

	/**
	 * This private class represents a node in the file system tree.
	 */
	private static abstract class Node {
		protected String name;
		protected int size;
		protected Node parent;
		protected Map<String, Node> childs;

		private Node(Node parent, String name) {
			this.name = name;
			this.size = 0;
			this.parent = parent;
			this.childs = null;
		}
	}

	/**
	 * This private class represents a directory in the file system tree.
	 */
	private static class NodeDir extends Node {
		private NodeDir(NodeDir parent, String name) {
			super(parent, name);
			super.childs = new HashMap<>();
		}
	}

	/**
	 * This private class represents a file in the file system tree.
	 */
	private static class NodeFile extends Node {
		private NodeFile(NodeDir parent, String name, int size) {
			super(parent, name);
			super.size = size;
		}
	}

	/**
	 * This recursive method updates all the directories sizes according to their content.
	 */
	private static void updateDirSizes(NodeDir node) {
		for (Node c: node.childs.values()) {
			if (c instanceof NodeDir) updateDirSizes((NodeDir) c);
			node.size += c.size;
		}
	}

	/**
	 * This recursive method searches all directories smaller in size than a limit.
	 */
	private static int sumSmallDirs(NodeDir node, int limit) {
		int total = 0;
		for (Node c: node.childs.values()) {
			if (c instanceof NodeDir) {
				if (c.size <= limit) total += c.size;
				total += sumSmallDirs((NodeDir) c, limit);
			}
		}
		return total;
	}

	/**
	 * This recursive method searches for the smaller directory over a limit.
	 */
	private static int findDirToDelete(NodeDir node, int need) {
		int result = -1;
		for (Node c: node.childs.values()) {
			if (c instanceof NodeDir) {
				int i = findDirToDelete((NodeDir) c, need);
				if (i != -1 && (result == -1 || result > i)) result = i;
			}
			if (c.size >= need && (result == -1 || result > c.size)) result = c.size;
		}
		return result;
	}

	/**
	 * This recursive method prints a node and every child it has.
	 */
	private static void printNode(Node node, int level) {
		for (int i = 0; i < level; i++) System.out.print("  ");
		System.out.println(node.name + ": " + node.size);
		if (node instanceof NodeDir)
			for (Node c: node.childs.values())
				printNode(c, level + 1);
	}

	public static void main(String[] args) throws IOException {
		NodeDir root = new NodeDir(null, "/");
		NodeDir current = null;

		// --- Reads and parse the input file ---
		try (BufferedReader reader = new BufferedReader(new FileReader(INPUT_FILE))) {
			String line;
			while ((line = reader.readLine()) != null) {
				if (line.startsWith("$ ")) {
					line = line.substring(2);
					if (line.startsWith("cd ")) {
						String name = line.substring(3);
						if (name.equals("/")) current = root;
						else if (name.equals("..")) current = (NodeDir) current.parent;
						else current = (NodeDir) current.childs.get(name);
					} else if (line.equals("ls")) continue;
					else throw new RuntimeException("Unsupported commmand: " + line);
				} else if (line.startsWith("dir ")) {
					String name = line.substring(4);
					Node dir = new NodeDir(current, name);
					current.childs.put(name, dir);
				} else if (line.matches("^\\d+\\s[\\w\\.]+$")) {
					String[] tokens = line.split(" ");
					String name = tokens[1];
					int size = Integer.parseInt(tokens[0]);
					Node file = new NodeFile(current, name, size);
					current.childs.put(name, file);
				}
			}
		}
		updateDirSizes(root);

		// --- Puzzle 1 ---
		int total = sumSmallDirs(root, LIMIT);
		System.out.format("1. The sum of the total sizes of the directories under %,d is %,d%n", LIMIT, total);

		// --- Puzzle 2 ---
		int need = UPDATE_SIZE - (DISK_SIZE - root.size);
		int size = findDirToDelete(root, need);
		System.out.format("2. The total size of the smalles directory needed is %,d%n",  size);
	}
}
