CC = gcc
CFLAGS = -I .
OUT_DIR = build

build: main.c
	mkdir -p $(OUT_DIR)
	$(CC) -o $(OUT_DIR)/main main.c

clean:
	rm -rf $(OUT_DIR)
