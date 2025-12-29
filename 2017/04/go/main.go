package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

// Checks if a passphrase is valid for the puzzle 1
func isValid1(s string) bool {
	set := make(map[string]struct{})
	for _, w := range strings.Fields(s) {
		if _, exists := set[w]; exists {
			return false
		}
		set[w] = struct{}{}
	}
	return true
}

// Checks if a passphrase is valid for the puzzle 2
func isValid2(s string) bool {
	set := make(map[string]struct{})
	for _, w := range strings.Fields(s) {
		runes := []rune(w)
		sort.Slice(runes, func(a, b int) bool {
			return runes[a] < runes[b]
		})
		wo := string(runes)
		if _, exists := set[wo]; exists {
			return false
		}
		set[wo] = struct{}{}
	}
	return true
}

func main() {
	// --- Variables ---
	count1 := 0
	count2 := 0

	// --- Read input file ---
	file, err := os.Open(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// --- Parse each line ---
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		// --- Puzzle 1 ---
		if isValid1(line) {
			count1++

			// --- Puzzle 2 ---
			if isValid2(line) {
				count2++
			}
		}
	}

	// --- Results ---
	fmt.Println("1. Valid passphrases:", count1)
	fmt.Println("2. Valid passphrases:", count2) // < 328
}
