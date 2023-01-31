CC = gcc
CFLAGS = -I .
OUT_DIR = build

build: main.c lib.c
	mkdir -p $(OUT_DIR)
	$(CC) -o $(OUT_DIR)/main *.c

run: build
	$(OUT_DIR)/main

clean:
	rm -rf $(OUT_DIR)
