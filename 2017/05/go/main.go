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
	// --- Variables ---
	program := []int{}

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
		n, err := strconv.Atoi(line)
		if err != nil {
			log.Panic(err)
		}
		program = append(program, n)
	}

	// --- Puzzle 1 ---
	idx := 0
	steps := 0
	code := slices.Clone(program)
	for idx < len(code) {
		code[idx]++
		idx += code[idx] - 1
		steps++
	}
	fmt.Println("1. Steps:", steps)

	// --- Puzzle 2 ---
	idx = 0
	steps = 0
	code = slices.Clone(program)
	for idx < len(code) {
		jmp := code[idx]
		if jmp >= 3 {
			code[idx]--
		} else {
			code[idx]++
		}
		idx += jmp
		steps++
	}
	fmt.Println("2. Steps:", steps)
}
