package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const DISK_SIZE_1 = 272
const DISK_SIZE_2 = 35651584

// Process a string
func process(a string) string {
	b := make([]rune, len(a))
	for i := 0; i < len(a); i++ {
		if a[i] == '1' {
			b[len(a)-i-1] = '0'
		} else {
			b[len(a)-i-1] = '1'
		}
	}
	return a + "0" + string(b)
}

// Generate the fill needed
func fill(seed string, length int) string {
	s := seed
	for {
		s = process(s)
		if len(s) >= length {
			break
		}
	}
	return s[:length]
}

// Calcualtes the checksum
func checksum(fill string) string {
	s := fill
	for {
		b := make([]rune, len(s)/2)
		for i := 0; i < len(s); i++ {
			if s[i] == s[i+1] {
				b[i/2] = '1'
			} else {
				b[i/2] = '0'
			}
			i++
		}
		if len(b)%2 != 0 {
			return string(b)
		}
		s = string(b)
	}
}

func main() {
	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	seed := strings.TrimSpace(string(bytes))

	// --- Puzzle 1 ---
	data := fill(seed, DISK_SIZE_1)
	sum := checksum(data)
	fmt.Println("1. Checksum:", sum)

	// --- Puzzle 2 ---
	data = fill(seed, DISK_SIZE_2)
	sum = checksum(data)
	fmt.Println("2. Checksum:", sum)
}
