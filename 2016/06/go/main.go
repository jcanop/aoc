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

// --- Structs ---
type item struct {
	key   rune
	count int
}

func main() {
	// --- Variables ---
	data := []map[rune]int{}
	result1 := ""
	result2 := ""

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
		if len(data) == 0 {
			for i := 0; i < len(line); i++ {
				data = append(data, make(map[rune]int))
			}
		}
		// --- Count occurrences ----
		for i, c := range line {
			data[i][c] += 1
		}
	}

	// --- Sort to find most and least frecuent character ---
	for i := 0; i < len(data); i++ {
		s := []item{}
		for k, c := range data[i] {
			s = append(s, item{key: k, count: c})
		}
		sort.Slice(s, func(i, j int) bool {
			return s[i].count > s[j].count
		})
		result1 += string(s[0].key)
		result2 += string(s[len(s)-1].key)
	}

	// --- Results ---
	fmt.Println("1. Message:", result1)
	fmt.Println("2. Message:", result2)
}
