package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// --- Constants ---
const INPUT_FILE = "../input/input.txt"

// Calcuates the used memory from a string
func countMem(s string) int {
	total := len(s) - 2
	i := 1
	for i < len(s)-2 {
		if s[i] == '\\' {
			if i+1 < len(s) && (s[i+1] == '\\' || s[i+1] == '"') {
				total--
				i++
			} else if i+2 < len(s) && s[i+1] == 'x' {
				total -= 3
				i += 3
			}
		}
		i++
	}
	return total
}

// Encode the string
func encode(s string) string {
	var ls []string
	ls = append(ls, `"`)
	for _, c := range s {
		if c == '"' || c == '\\' {
			ls = append(ls, "\\")
		}
		ls = append(ls, string(c))
	}
	ls = append(ls, `"`)
	return strings.Join(ls, "")
}

func main() {
	// --- Variables ---
	total1 := 00
	total2 := 0

	// --- Read input file ---
	f, err := os.Open(INPUT_FILE)
	if err != nil {
		fmt.Println("Error al abrir archivo:", err)
		return
	}
	defer f.Close()

	// --- Parse each line ---
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()

		// --- Puzzle 1 ---
		mem := countMem(line)
		total1 += len(line) - mem

		// --- Puzzle 2 ---
		enc := encode(line)
		total2 += len(enc) - countMem(enc)
	}

	// --- Print results ---
	fmt.Printf("1. Number of characters in memory: %d\n", total1)
	fmt.Printf("2. Number of characters in memory: %d\n", total2)
}
