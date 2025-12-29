package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

func lookAndSay(s string) string {
	if len(s) == 0 {
		return ""
	}

	var res strings.Builder
	a := []byte(s)
	last := a[0]
	count := 1

	for i := 1; i < len(a); i++ {
		if a[i] == last {
			count++
		} else {
			res.WriteString(strconv.Itoa(count))
			res.WriteByte(last)
			last = a[i]
			count = 1
		}
	}
	res.WriteString(strconv.Itoa(count))
	res.WriteByte(last)
	return res.String()
}

func main() {
	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))

	// --- Puzzle 1 ---
	for i := 0; i < 40; i++ {
		text = lookAndSay(text)
	}
	fmt.Println("1. Length of the result:", len(text))

	// --- Puzzle 2 ---
	for i := 0; i < 10; i++ {
		text = lookAndSay(text)
	}
	fmt.Println("2. Length of the result:", len(text))
}
