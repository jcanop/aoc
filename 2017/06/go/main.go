package main

import (
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
	contains := func(s [][]int, v []int) bool {
		for _, r := range s {
			if slices.Equal(r, v) {
				return true
			}
		}
		return false
	}

	// --- Variables ---
	banks := []int{}

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))
	for _, f := range strings.Fields(text) {
		n, err := strconv.Atoi(f)
		if err != nil {
			log.Panic(err)
		}
		banks = append(banks, n)
	}

	// --- Runs redistribution ---
	bs := slices.Clone(banks)
	set := [][]int{}
	cycles1 := 0
	cycles2 := 0
	var loop *[]int
	for {
		mx := slices.Max(bs)
		idx := slices.Index(bs, mx)
		bs[idx] = 0
		for i := 1; i <= mx; i++ {
			j := (idx + i) % len(bs)
			bs[j]++
		}
		if loop == nil {
			cycles1++
		} else {
			cycles2++
		}

		state := slices.Clone(bs)
		if loop == nil {
			if contains(set, state) {
				loop = &state
			}
			set = append(set, state)
		} else if slices.Equal(state, *loop) {
			break
		}
	}

	// --- Prints Results ---
	fmt.Println("1. Cycles:", cycles1)
	fmt.Println("2. Cycles:", cycles2)
}
