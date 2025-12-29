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
const TEST_LEN = 100

// Executes the code
func execute(code []string, regs []int) bool {
	last := -1
	count := 0
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
		case "out":
			j, err := strconv.Atoi(fields[1])
			if err != nil {
				j = regs[r1]
			}
			if last != -1 && last == j {
				return false
			}
			last = j
			count++
			if count == TEST_LEN {
				return true
			}
		}
	}
	return false
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

	// --- Puzzle ---
	regs := []int{0, 0, 0, 0}
	i := 0
	for {
		regs[0] = i
		regs[1] = 0
		regs[2] = 0
		regs[3] = 0
		result := execute(code, regs)
		if result {
			break
		}
		i++
	}
	fmt.Println("1. Value in register a:", i)
}
