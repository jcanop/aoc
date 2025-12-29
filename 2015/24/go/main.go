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

// Generate all the combinations of a N size slice.
func combinationsN[T any](array []T, n int) [][]T {
	var result [][]T

	var backtrack func(start int, comb []T)
	backtrack = func(start int, comb []T) {
		if len(comb) == n {
			c := make([]T, n)
			copy(c, comb)
			result = append(result, c)
			return
		}

		for i := start; i <= len(array)-(n-len(comb)); i++ {
			backtrack(i+1, append(comb, array[i]))
		}
	}

	backtrack(0, []T{})
	return result
}

func quantumEntanglement(list []int, groups int) int {
	mn := math.MaxInt
	target := 0
	for _, v := range list {
		target += v
	}
	target /= groups

	for i := 1; i < len(list); i++ {
		for _, c := range combinationsN(list, i) {
			sum := 0
			prod := 1
			for _, v := range c {
				sum += v
				prod *= v
			}
			if sum == target {
				mn = min(mn, prod)

			}
		}
		if mn != math.MaxInt {
			return mn
		}
	}
	return math.MinInt
}

func main() {
	// --- Variables ---
	list := []int{}

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
		list = append(list, n)
	}

	// --- Puzzle 1 ---
	qe := quantumEntanglement(list, 3)
	fmt.Println("1. Quantum entanglement:", qe)

	// --- Puzzle 2 ---
	qe = quantumEntanglement(list, 4)
	fmt.Println("2. Quantum entanglement:", qe)
}
