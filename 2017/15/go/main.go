package main

import (
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const SAMPLE_SIZE_1 = 40_000_000
const SAMPLE_SIZE_2 = 5_000_000
const FACTOR_A int64 = 16807
const FACTOR_B int64 = 48271
const DIVISOR int64 = 2147483647
const CRITERIA_A int64 = 4
const CRITERIA_B int64 = 8

func main() {
	// --- Helper functions ---
	parseInt := func(s string) int64 {
		n, err := strconv.ParseInt(s, 10, 64)
		if err != nil {
			log.Panic(err)
		}
		return n
	}

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))
	text = strings.ReplaceAll(text, "\n", " ")
	re := regexp.MustCompile(`\d+`)
	m := re.FindAllString(text, -1)
	inputA := parseInt(m[0])
	inputB := parseInt(m[1])

	// --- Puzzle 1 ---
	next := func(prev int64, id rune) int64 {
		var factor int64
		switch id {
		case 'a':
			factor = FACTOR_A
		case 'b':
			factor = FACTOR_B
		default:
			log.Panic("Invlid id:", id)
		}
		return (prev * factor) % DIVISOR
	}
	a := inputA
	b := inputB
	matches := 0
	for i := 0; i < SAMPLE_SIZE_1; i++ {
		a = next(a, 'a')
		b = next(b, 'b')
		if a&0xFFFF == b&0xFFFF {
			matches++
		}
	}
	fmt.Println("1. Matches:", matches)

	// --- Puzzle 2 ---
	nextRestricted := func(prev int64, id rune) int64 {
		result := next(prev, id)
		for {
			if id == 'a' && result%CRITERIA_A == 0 {
				return result
			}
			if id == 'b' && result%CRITERIA_B == 0 {
				return result
			}
			result = next(result, id)
		}
	}
	a = inputA
	b = inputB
	matches = 0
	for i := 0; i < SAMPLE_SIZE_2; i++ {
		a = nextRestricted(a, 'a')
		b = nextRestricted(b, 'b')
		if a&0xFFFF == b&0xFFFF {
			matches++
		}
	}
	fmt.Println("2. Matches:", matches)
}
