package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

// Executes the code
func execute(code []string, regs []int) {
	for i := 0; i < len(code); i++ {
		fields := strings.Fields(code[i])
		r1 := fields[1][0] - 'a'
		switch fields[0] {
		case "cpy":
			r2 := fields[2][0] - 'a'
			n, err := strconv.Atoi(fields[1])
			if err != nil {
				regs[r2] = regs[r1]
			} else {
				regs[r2] = n
			}
		case "inc":
			regs[r1]++
		case "dec":
			regs[r1]--
		case "jnz":
			j, err := strconv.Atoi(fields[2])
			if err != nil {
				j = regs[fields[2][0]-'a']
			}

			n, err := strconv.Atoi(fields[1])
			if err != nil {
				if regs[r1] != 0 {
					i = i + j - 1
				}
			} else {
				if n != 0 {
					i = i + j - 1
				}
			}
		}
	}
}

func main() {
	// --- Variables ---
	code := []string{}

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
		code = append(code, line)
	}

	// --- Puzzle 1 ---
	regs := make([]int, 4)
	execute(code, regs)
	fmt.Println("1. Value in register a:", regs[0])

	// --- Puzzle 2 ---
	regs = []int{0, 0, 1, 0}
	execute(code, regs)
	fmt.Println("2. Value in register a:", regs[0])
}
