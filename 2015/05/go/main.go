package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

var LIST1 = []rune{'a', 'e', 'i', 'o', 'u'}
var LIST2 = []string{"ab", "cd", "pq", "xy"}

// --- Rules ---
func rule11(s string) bool {
	count := 0
	for _, c := range s {
		if slices.Contains(LIST1, c) {
			count++
		}
		if count == 3 {
			return true
		}
	}
	return false
}

func rule12(s string) bool {
	a := []rune(s)
	for i := 0; i < len(a)-1; i++ {
		if a[i] == a[i+1] {
			return true
		}
	}
	return false
}

func rule13(s string) bool {
	for _, c := range LIST2 {
		if strings.Contains(s, c) {
			return false
		}
	}
	return true
}

func rule21(s string) bool {
	a := []rune(s)
	for i := 0; i < len(a)-2; i++ {
		for j := i + 2; j < len(a)-1; j++ {
			if a[i] == a[j] && a[i+1] == a[j+1] {
				return true
			}
		}
	}
	return false
}

func rule22(s string) bool {
	a := []rune(s)
	for i := 0; i < len(a)-2; i++ {
		if a[i] == a[i+2] {
			return true
		}
	}
	return false
}

func main() {
	// --- Variables --
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
		if rule11(line) && rule12(line) && rule13(line) {
			count1++
		}
		if rule21(line) && rule22(line) {
			count2++
		}
	}

	// --- Results ---
	fmt.Println("1. Nice strings:", count1)
	fmt.Println("2. Nice strings:", count2)
}
