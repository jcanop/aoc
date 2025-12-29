package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const REGEX = `(\d+)x(\d+)x(\d+)`

func main() {
	// --- Helper functions ---
	parseInt := func(s string) int {
		n, err := strconv.Atoi(s)
		if err != nil {
			log.Panic(err)
		}
		return n
	}

	// --- Variables ---
	re := regexp.MustCompile(REGEX)
	total := 0
	ribbon := 0

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
		m := re.FindStringSubmatch(line)
		l := parseInt(m[1])
		w := parseInt(m[2])
		h := parseInt(m[3])

		// --- Puzzle 1 ---
		a := l * w
		b := w * h
		c := l * h
		n := min(a, b, c)
		total += 2*a + 2*b + 2*c + n

		// --- Puzzle 2 ---
		n = max(l, w, h)
		ribbon += 2*(l+w+h-n) + l*w*h
	}

	// --- Results ---
	fmt.Println("1. Total square feet of wrapping paper:", total)
	fmt.Println("2. Total feet of ribbon:", ribbon)
}
