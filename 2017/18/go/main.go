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
const THREADS = 2

// --- Helper functions ---
func parseInt(s string) int {
	n, err := strconv.Atoi(s)
	if err != nil {
		log.Panic(err)
	}
	return n
}

// Execute the code for puzzle 1
func execute1(code []string) int {
	regs := make(map[string]int)
	freq := -1

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
		case "snd":
			freq = value(s[1])
		case "set":
			regs[s[1]] = value(s[2])
		case "add":
			regs[s[1]] += value(s[2])
		case "mul":
			regs[s[1]] *= value(s[2])
		case "mod":
			regs[s[1]] %= value(s[2])
		case "rcv":
			if value(s[1]) != 0 {
				return freq
			}
		case "jgz":
			if value(s[1]) > 0 {
				idx += value(s[2]) - 1
			}
		}
		idx++
	}
	return -1
}

// Execute the code for puzzle 2
func execute2(code []string) int {
	idxs := [THREADS]int{}
	regs := [THREADS]map[string]int{
		make(map[string]int),
		make(map[string]int),
	}
	regs[0]["p"] = 0
	regs[1]["p"] = 1
	queues := [THREADS][]int{}
	count := 0
	c := 0

	value := func(s string) int {
		v, err := strconv.Atoi(s)
		if err != nil {
			v = regs[c][s]
		}
		return v
	}

	deadlock := func() bool {
		for i := 0; i < THREADS; i++ {
			if len(queues[i]) > 0 {
				return false
			}
		}
		return true
	}

	for idxs[c] < len(code) {
		s := strings.Fields(code[idxs[c]])
		switch s[0] {
		case "snd":
			d := (c + 1) % THREADS
			queues[d] = append(queues[d], value(s[1]))
			if c == 1 {
				count++
			}
		case "set":
			regs[c][s[1]] = value(s[2])
		case "add":
			regs[c][s[1]] += value(s[2])
		case "mul":
			regs[c][s[1]] *= value(s[2])
		case "mod":
			regs[c][s[1]] %= value(s[2])
		case "rcv":
			if deadlock() {
				return count
			}
			if len(queues[c]) == 0 {
				c = (c + 1) % THREADS
				idxs[c]--
			} else {
				var v int
				v, queues[c] = queues[c][0], queues[c][1:]
				regs[c][s[1]] = v
			}
		case "jgz":
			if value(s[1]) > 0 {
				idxs[c] += value(s[2]) - 1
			}
		}
		idxs[c]++
	}
	return count
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
	freq := execute1(code)
	fmt.Println("1. Recovered frequency:", freq)

	// --- Puzzle 2 ---
	count := execute2(code)
	fmt.Println("2. Times did program 1 send a value:", count)
}
