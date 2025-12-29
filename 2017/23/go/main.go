package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

// Execute the code for puzzle 1
func execute(code []string, puzzle int) int {
	regs := make(map[string]int)
	if puzzle == 2 {
		regs["a"] = 1
	}
	count := 0

	value := func(s string) int {
		v, err := strconv.Atoi(s)
		if err != nil {
			v = regs[s]
		}
		return v
	}

	idx := 0
	for idx < len(code) {
		s := strings.Fields(code[idx])
		switch s[0] {
		case "set":
			regs[s[1]] = value(s[2])
		case "add":
			regs[s[1]] += value(s[2])
		case "sub":
			regs[s[1]] -= value(s[2])
		case "mul":
			regs[s[1]] *= value(s[2])
			count++
		case "jnz":
			if value(s[1]) != 0 {
				idx += value(s[2]) - 1
			}
		}
		idx++

		if puzzle == 2 && idx == 11 {
			break
		}
	}

	switch puzzle {
	case 1:
		return count
	case 2:
		np := (regs["c"]-regs["b"])/34 + 1
		for b := regs["b"] + 17; b <= regs["c"]; b += 34 {
			isComposite := false
			limit := int(math.Sqrt(float64(b)))
			for d := 3; d <= limit; d += 2 {
				if b%d == 0 {
					isComposite = true
					break
				}
			}
			if isComposite {
				np++
			}
		}
		return np
	default:
		log.Panic("Unsupported puzzle:", puzzle)
	}
	return -1
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
	count := execute(code, 1)
	fmt.Println("1. Times mul is invoked:", count)

	// --- Puzzle 2 ---
	h := execute(code, 2)
	fmt.Println("2. Register h:", h)
}
