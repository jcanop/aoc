package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const LEN = 128
const HASH_LEN = 256
const HASH_ROUNDS = 64
const HASH_DENSE_LEN = 16

// Calculates the Knot Hash
func knotHash(s string) [HASH_DENSE_LEN]byte {
	list := make([]int, HASH_LEN)
	for i := 0; i < HASH_LEN; i++ {
		list[i] = i
	}
	input := make([]int, len(s))
	for i, c := range s {
		input[i] = int(c)
	}
	input = append(input, 17, 31, 73, 47, 23)

	idx := 0
	skip := 0
	for r := 0; r < HASH_ROUNDS; r++ {
		for _, i := range input {
			for j := 0; j < i/2; j++ {
				a := (idx + j) % HASH_LEN
				b := (idx - j + i - 1) % HASH_LEN
				list[a], list[b] = list[b], list[a]
			}
			idx = (idx + i + skip) % HASH_LEN
			skip++
		}
	}
	dense := [HASH_DENSE_LEN]byte{}
	for i := 0; i < HASH_DENSE_LEN; i++ {
		dense[i] = byte(list[i*HASH_DENSE_LEN])
		for j := 1; j < HASH_DENSE_LEN; j++ {
			dense[i] = dense[i] ^ byte(list[i*HASH_DENSE_LEN+j])
		}
	}
	return dense
}

func main() {
	// --- Variables ---
	disk := [LEN][HASH_DENSE_LEN]byte{}

	// --- Helper functions ---
	get := func(x, y int) bool {
		mask := byte(0x1)
		b := disk[y][x/8]
		i := 8 - (x % 8) - 1
		if (b>>i)&mask == mask {
			return true
		}
		return false
	}

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))

	// --- Fill the disk ---
	for i := 0; i < LEN; i++ {
		disk[i] = knotHash(text + "-" + strconv.Itoa(i))
	}

	// --- Puzzle 1 ---
	used := 0
	for y := 0; y < LEN; y++ {
		for x := 0; x < LEN; x++ {
			if get(x, y) {
				used++
			}
		}
	}
	fmt.Println("1. Used:", used)

	// --- Puzzle 2 ---
	visited := [LEN][LEN]bool{}
	var dfs func(int, int)
	dfs = func(x, y int) {
		if x < 0 || x >= LEN || y < 0 || y >= LEN || !get(x, y) || visited[x][y] {
			return
		}
		visited[x][y] = true
		dfs(x-1, y)
		dfs(x+1, y)
		dfs(x, y-1)
		dfs(x, y+1)
	}
	regions := 0
	for y := 0; y < LEN; y++ {
		for x := 0; x < LEN; x++ {
			if get(x, y) && !visited[x][y] {
				dfs(x, y)
				regions++
			}
		}
	}
	fmt.Println("2. Regions:", regions)
}
