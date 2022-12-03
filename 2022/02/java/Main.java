import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Main {
	private static final String INPUT_FILE = "../input/input.txt";

	/**
	 * This enumeration represents the result of a game.
	 */
	private static enum Result {
		LOSE('X', 0), DRAW('Y', 3), WIN('Z', 6);

		private final char id;
		private final int points;

		private Result(char id, int points) {
			this.id = id;
			this.points = points;
		}

		private static Result parse(char id) {
			for (Result r: Result.values()) if (r.id == id) return r;
			throw new IllegalArgumentException("Invalid result with id: " + id);
		}
	}

	/**
	 * This enumeration represents the shapes of the game: rock, paper, scissors.
	 */
	private static enum Shape {
		ROCK('A', 'X', 1), PAPER('B', 'Y', 2), SCISSORS('C', 'Z', 3);

		private final char id1;
		private final char id2;
		private final int points;

		private Shape(char id1, char id2, int points) {
			this.id1 = id1;
			this.id2 = id2;
			this.points = points;
		}

		private static Shape parse(char id) {
			for (Shape s: Shape.values()) if (s.id1 == id || s.id2 == id) return s;
			throw new IllegalArgumentException("Invalid shape with id: " + id);
		}

		/**
		 * This method returns the shape to win, lose or draw against this shape.
		 *
		 * @param result Desired result.
		 * @return The shape to play.
		 */
		public Shape shapeTo(Result result) {
			switch(result) {
				case WIN:
					switch(this) {
						case ROCK: return Shape.PAPER;
						case PAPER: return Shape.SCISSORS;
						case SCISSORS: return Shape.ROCK;
					}
				case LOSE:
					switch(this) {
						case ROCK: return Shape.SCISSORS;
						case PAPER: return Shape.ROCK;
						case SCISSORS: return Shape.PAPER;
					}
				case DRAW: return this;
			}
			throw new IllegalArgumentException("Unsupported result: " + result);
		}

		/**
		 * This method matches two shapes and returns a result.
		 *
		 * @param opponent Opponent's shape.
		 * @return The result of the match.
		 */
		public Result match(Shape opponent) {
			if (this == opponent) return Result.DRAW;
			if ((this == ROCK && opponent == SCISSORS) ||
				(this == SCISSORS && opponent == PAPER) ||
				(this == PAPER && opponent == ROCK)) return Result.WIN;
			return Result.LOSE;
		}

		/**
		 * This method plays the game against this shape.
		 *
		 * @param opponent Opponent's shape.
		 * @return The points result of the match.
		 */
		private int play(Shape opponent) {
			int result = this.points;
			switch (this.match(opponent)) {
				case DRAW: result += 3; break;
				case WIN: result += 6;
			}
			return result;
		}
	}

	public static void main(String[] args) throws IOException {
		// --- Puzzle: Part 1 ---
		try (Stream<String> stream = Files.lines(Paths.get(INPUT_FILE))) {
			int total = stream.map(line -> {
				Shape op = Shape.parse(line.charAt(0));
				Shape me = Shape.parse(line.charAt(2));
				return me.play(op);
			}).reduce(0, (acc, points) -> acc + points);
			System.out.format("Part 1. Total score: %,d%n", total);
		}

		// --- Puzzle: Part 2 ---
		try (Stream<String> stream = Files.lines(Paths.get(INPUT_FILE))) {
			int total = stream.map(line -> {
				Shape op = Shape.parse(line.charAt(0));
				Result result = Result.parse(line.charAt(2));
				Shape me = op.shapeTo(result);
				return me.play(op);
			}).reduce(0, (acc, points) -> acc + points);
			System.out.format("Part 2. Total score: %,d%n", total);
		}
	}
}
