package main

import (
	"fmt"
	"log"
	"os"
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

	floor := 0
	first := 0
	for i, c := range text {
		// --- Puzzle 1 ---
		if c == '(' {
			floor++
		} else {
			floor--
		}

		// --- Puzzle 2 ---
		if first == 0 && floor == -1 {
			first = i + 1
		}
	}

	// --- Results ---
	fmt.Println("1. Floor:", floor)
	fmt.Println("2. Position:", first)
}
