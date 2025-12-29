package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const INPUT_FILE = "../input/input.txt"

func solve(op string, wires map[string]string, cache map[string]int) int {
	if n, err := strconv.Atoi(op); err == nil {
		return n
	}
	if v, ok := cache[op]; ok {
		return v
	}

	_solve := func(o string) int {
		return solve(o, wires, cache)
	}

	var r int
	ls := strings.Fields(wires[op])
	switch {
	case contains(ls, "AND"):
		r = _solve(ls[0]) & _solve(ls[2])
	case contains(ls, "OR"):
		r = _solve(ls[0]) | _solve(ls[2])
	case contains(ls, "LSHIFT"):
		r = _solve(ls[0]) << _solve(ls[2])
	case contains(ls, "RSHIFT"):
		r = _solve(ls[0]) >> _solve(ls[2])
	case contains(ls, "NOT"):
		r = ^_solve(ls[1])
	default:
		r = _solve(ls[0])
	}
	cache[op] = r
	return r
}

func contains(arr []string, s string) bool {
	for _, v := range arr {
		if v == s {
			return true
		}
	}
	return false
}

func main() {
	// --- Variables --
	wires := make(map[string]string)

	// --- Read input file ---
	f, err := os.Open(INPUT_FILE)
	if err != nil {
		fmt.Println("Erro ao abrir o arquivo:", err)
		return
	}
	defer f.Close()

	// --- Parse each line ---
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " -> ")
		if len(parts) != 2 {
			continue
		}
		expr := strings.TrimSpace(parts[0])
		target := strings.TrimSpace(parts[1])
		wires[target] = expr
	}
	if err := scanner.Err(); err != nil {
		fmt.Println(err)
		return
	}

	// --- Puzzle 1 ---
	cache := make(map[string]int)
	r := solve("a", wires, cache)
	fmt.Printf("1. Wire a: %d\n", r)

	// --- Puzzle 2 ---
	wires["b"] = strconv.Itoa(r)
	cache = make(map[string]int)
	r = solve("a", wires, cache)
	fmt.Printf("2. Wire a: %d\n", r)
}
