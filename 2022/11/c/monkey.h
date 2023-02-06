// --- Constants ---
#define ITEMS_MAX 100

// --- Structs ---
struct Monkey {
	int id;
	long items[ITEMS_MAX];
	int items_idx;
	char operator;
	long operand;
	long divisible;
	int throw_true;
	int throw_false;
	long inspects;
};

// Create a new Monkey struct
struct Monkey* monkey_create(void);

// Adds an item to a monkey
void monkey_add_item(struct Monkey* m, long item);

// Remove the last item from a monkeys list
long monkey_pop_item(struct Monkey* m);

// Prints a Monkey struct
void monkey_print(const struct Monkey* m);
