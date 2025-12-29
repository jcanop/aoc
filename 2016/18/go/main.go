package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const ROWS_1 = 40
const ROWS_2 = 400000

// Parse the text input into a slide of booleans where true is a trap
func parseLine(s string) []bool {
	a := []bool{}
	for _, c := range s {
		a = append(a, c == '^')
	}
	return a
}

// Calculates the next line
func next(a []bool) []bool {
	b := make([]bool, len(a))
	for i := 0; i < len(a); i++ {
		if i == 0 {
			b[i] = a[i+1]
		} else if i == len(a)-1 {
			b[i] = a[i-1]
		} else {
			b[i] = (a[i-1] && !a[i+1]) || (!a[i-1] && a[i+1])
		}
	}
	return b
}

// Count the safe titles of a row
func countSafe(a []bool) int {
	count := 0
	for _, v := range a {
		if !v {
			count++
		}
	}
	return count
}

func main() {
	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	line := strings.TrimSpace(string(bytes))

	// --- Puzzle 1 ---
	a := parseLine(line)
	count := countSafe(a)
	for i := 1; i < ROWS_1; i++ {
		a = next(a)
		count += countSafe(a)
	}
	fmt.Println("1. Safe titles:", count)

	// --- Puzzle 2 ---
	a = parseLine(line)
	count = countSafe(a)
	for i := 1; i < ROWS_2; i++ {
		a = next(a)
		count += countSafe(a)
	}
	fmt.Println("2. Safe titles:", count)
}
