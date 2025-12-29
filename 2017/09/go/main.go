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

	// --- Process the stream ---
	group := 0
	garbage := false
	skip := false
	total := 0
	count := 0
	for _, c := range text {
		if skip {
			skip = false
		} else if c == '!' {
			skip = true
		} else if !garbage {
			switch c {
			case '{':
				group++
				total += group
			case '}':
				group--
			case '<':
				garbage = true
			}
		} else {
			if c == '>' {
				garbage = false
			} else {
				count++
			}
		}
	}

	// --- Results ---
	fmt.Println("1. Total score:", total)
	fmt.Println("2. Garbage characters:", count)
}
