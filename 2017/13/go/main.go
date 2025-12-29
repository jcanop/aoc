package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

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
	ranges := make(map[int]int)
	ln := -1

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
		m := strings.Split(line, ": ")
		key := parseInt(m[0])
		val := parseInt(m[1])
		ranges[key] = val
		ln = max(ln, key)
	}
	ln++

	// --- Puzzle 1: Sumulating ---
	idx := 0
	severity := 0
	levels := make([]int, ln)
	dirs := slices.Repeat([]int{1}, ln)
	for idx < ln {
		if levels[idx] == 0 {
			severity += idx * ranges[idx]
		}
		for k, v := range ranges {
			if levels[k] == 0 {
				dirs[k] = 1
			} else if levels[k] == v-1 {
				dirs[k] = -1
			}
			levels[k] += dirs[k]
		}
		idx++
	}
	fmt.Println("1. Severity:", severity)

	// --- Puzzle 2: Calculing ---
	find := func(firewall map[int]int) int {
		delay := 0
		nextCatch := make([]int, ln)
		for i := range nextCatch {
			nextCatch[i] = -1
		}
		for depth, r := range firewall {
			nextCatch[depth] = (r - 1) * 2
		}
		for {
			caught := false
			for depth, period := range nextCatch {
				if period != -1 && (depth+delay)%period == 0 {
					caught = true
					break
				}
			}
			if !caught {
				return delay
			}
			delay++
		}
	}
	delay := find(ranges)
	fmt.Println("2. Delay:", delay)
}
