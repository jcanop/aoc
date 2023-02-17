CC = gcc
COMMONS = ../../../commons/c
CFLAGS = -I . -I $(COMMONS) -O3
OUT_DIR = build

build: main.c
	mkdir -p $(OUT_DIR)
	$(CC) -o $(OUT_DIR)/main $(CFLAGS) *.c

run: build
	$(OUT_DIR)/main

valgrind: build
	valgrind --tool=memcheck --leak-check=full --show-leak-kinds=all --track-origins=yes -s $(OUT_DIR)/main

lib:
	mkdir -p $(OUT_DIR)
	$(CC) -o $(OUT_DIR)/libc.so $(CFLAGS) -fPIC -shared lib.c

clean:
	rm -rf $(OUT_DIR)
