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
	ln := len(text)

	// --- Puzzle 1 ---
	sum := 0
	for i := 0; i < ln; i++ {
		j := (i + 1) % ln
		if text[i] == text[j] {
			sum += int(text[i] - '0')
		}
	}
	fmt.Println("1. Solution:", sum)

	// --- Puzzle 2 ---
	sum = 0
	j := ln / 2
	for i := 0; i < j; i++ {
		if text[i] == text[j+i] {
			sum += int(text[i]-'0') * 2
		}
	}
	fmt.Println("2. Solution:", sum)
}
