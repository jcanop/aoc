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

// Executes the code
func execute(instructions []string, regs []int) {
	code := slices.Clone(instructions)

	for i := 0; i < len(code); i++ {
		fields := strings.Fields(code[i])
		r1 := byte(0)
		if len(fields) > 1 {
			r1 = fields[1][0] - 'a'
		}
		switch fields[0] {
		case "tgl":
			j := i + regs[r1]
			if j >= len(code) {
				continue
			}
			fs := strings.Fields(code[j])
			switch fs[0] {
			case "cpy":
				code[j] = strings.ReplaceAll(code[j], "cpy", "jnz")
			case "inc":
				code[j] = strings.ReplaceAll(code[j], "inc", "dec")
			case "dec":
				code[j] = strings.ReplaceAll(code[j], "dec", "inc")
			case "jnz":
				code[j] = strings.ReplaceAll(code[j], "jnz", "cpy")
			case "tgl":
				code[j] = strings.ReplaceAll(code[j], "tgl", "inc")
			}
		case "cpy":
			n, err := strconv.Atoi(fields[2])
			if err == nil {
				continue
			}
			r2 := fields[2][0] - 'a'
			n, err = strconv.Atoi(fields[1])
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
		case "nop":
			// Ignore
		case "mul":
			r2 := fields[2][0] - 'a'
			regs[r1] *= regs[r2]
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
	regs[0] = 7
	execute(code, regs)
	fmt.Println("1. Value in register a:", regs[0])

	// --- Puzzle 2 ---
	regs = make([]int, 4)
	regs[0] = 12
	code[5] = "nop"
	code[6] = "mul c d"
	code[7] = "cpy c a"
	code[8] = "cpy 0 c"
	code[9] = "cpy 0 d"
	execute(code, regs)
	fmt.Println("2. Value in register a:", regs[0])
}
