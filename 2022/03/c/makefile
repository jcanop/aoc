CC = gcc
CFLAGS = -I . -I ../../../commons/c -O3
OUT_DIR = build

build: main.c
	mkdir -p $(OUT_DIR)
	$(CC) -o $(OUT_DIR)/main $(CFLAGS) *.c

run: build
	$(OUT_DIR)/main

clean:
	rm -rf $(OUT_DIR)
