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
const REGEX = `(\w+) (inc|dec) (-?\d+) if (\w+) (<|<=|==|>=|>|!=) (-?\d+)`

func main() {
	// --- Variables ---
	re := regexp.MustCompile(REGEX)
	regs := make(map[string]int)
	highest := -1

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
		reg1 := m[1]
		op := m[2]
		val1, _ := strconv.Atoi(m[3])
		reg2 := m[4]
		cond := m[5]
		val2, _ := strconv.Atoi(m[6])

		// --- Evaluate if the instuction will execute ---
		switch cond {
		case "<":
			if regs[reg2] >= val2 {
				continue
			}
		case "<=":
			if regs[reg2] > val2 {
				continue
			}
		case "==":
			if regs[reg2] != val2 {
				continue
			}
		case ">=":
			if regs[reg2] < val2 {
				continue
			}
		case ">":
			if regs[reg2] <= val2 {
				continue
			}
		case "!=":
			if regs[reg2] == val2 {
				continue
			}
		default:
			log.Panic("Invalid conditional:", cond)
		}

		// --- Execute the instruction ---
		switch op {
		case "inc":
			regs[reg1] += val1
			highest = max(highest, regs[reg1])
		case "dec":
			regs[reg1] -= val1
		default:
			log.Panic("Invalid op:", op)
		}
	}

	// --- Puzzle 1 ---
	mx := -1
	for _, v := range regs {
		mx = max(mx, v)
	}
	fmt.Println("1. Largest value:", mx)

	// --- Puzzle 2 ---
	fmt.Println("2. Highest value:", highest)
}
