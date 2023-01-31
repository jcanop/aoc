// --- Enums ---
enum Shape {
	ROCK = 1, PAPER = 2, SCISSORS = 3
};

enum Result {
	WIN = 6, DRAW = 3, LOSE = 0
};

// --- Enums parsers ---
enum Shape parse_shape(char c);
enum Result parse_result(char c);


// --- Play methods ---
enum Result play(enum Shape me, enum Shape other);
enum Shape shape_to(enum Shape other, enum Result result);
