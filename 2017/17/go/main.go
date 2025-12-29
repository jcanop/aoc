package main

import (
	"fmt"
	"log"
	"os"
	"slices"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

func main() {
	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))
	input, err := strconv.Atoi(text)
	if err != nil {
		log.Panic(err)
	}
	//input = 3

	// --- Puzzle 1 ---
	buffer := []int{0}
	idx := 0
	for i := 1; i <= 2017; i++ {
		idx = (idx+input)%i + 1
		buffer = slices.Insert(buffer, idx, i)
	}
	fmt.Println("1. Value after 2017:", buffer[(idx+1)%len(buffer)])

	// --- Puzzle 2 ---
	var n int
	for i := 1; i <= 50_000_000; i++ {
		idx = (idx+input)%i + 1
		if idx == 1 {
			n = i
		}
	}
	fmt.Println("2. Value after 0:", n)
}
