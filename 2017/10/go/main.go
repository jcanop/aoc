package main

import (
	"encoding/hex"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const LEN = 256
const ROUNDS = 64
const DENSE_LEN = 16

// Calculates the hash for the first puzzle
func hash1(list []int, input []int) {
	idx := 0
	skip := 0
	for _, i := range input {
		for j := 0; j < i/2; j++ {
			a := (idx + j) % LEN
			b := (idx - j + i - 1) % LEN
			list[a], list[b] = list[b], list[a]
		}
		idx = (idx + i + skip) % LEN
		skip++
	}
}

// Calculates the hash for the second puzzle
func hash2(list []int, input []int) string {
	idx := 0
	skip := 0
	for r := 0; r < ROUNDS; r++ {
		for _, i := range input {
			for j := 0; j < i/2; j++ {
				a := (idx + j) % LEN
				b := (idx - j + i - 1) % LEN
				list[a], list[b] = list[b], list[a]
			}
			idx = (idx + i + skip) % LEN
			skip++
		}
	}
	dense := make([]byte, LEN/DENSE_LEN)
	for i := 0; i < LEN/DENSE_LEN; i++ {
		dense[i] = byte(list[i*DENSE_LEN])
		for j := 1; j < DENSE_LEN; j++ {
			dense[i] = dense[i] ^ byte(list[i*DENSE_LEN+j])
		}
	}
	return hex.EncodeToString(dense)
}

func main() {
	// --- Variables ---
	list := make([]int, LEN)

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))

	// --- Puzzle 1 ---
	for i := 0; i < LEN; i++ {
		list[i] = i
	}
	input := []int{}
	for _, t := range strings.Split(text, ",") {
		n, err := strconv.Atoi(strings.TrimSpace(t))
		if err != nil {
			log.Panic(err)
		}
		input = append(input, n)
	}
	hash1(list, input)
	fmt.Println("1. Result of multyplying the first two numbers:", list[0]*list[1])

	// --- Puzzle 2 ---
	for i := 0; i < LEN; i++ {
		list[i] = i
	}
	input = []int{}
	for _, c := range text {
		input = append(input, int(c))
	}
	input = append(input, 17, 31, 73, 47, 23)
	hash := hash2(list, input)
	fmt.Println("2. Knot Hash:", hash)
}
