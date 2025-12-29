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
const LITERS = 150

// Generate all the combinations
func generateCombinations(nums []int, n int) [][]int {
	var result [][]int
	comb := make([]int, 0, n)
	generate(&result, nums, comb, 0, n)
	return result
}

func generate(result *[][]int, nums []int, comb []int, start, n int) {
	if len(comb) == n {
		*result = append(*result, append([]int{}, comb...))
		return
	}

	for i := start; i < len(nums); i++ {
		comb = append(comb, nums[i])
		generate(result, nums, comb, i+1, n)
		comb = comb[:len(comb)-1] // backtrack
	}
}

func sum(nums []int) int {
	total := 0
	for _, num := range nums {
		total += num
	}
	return total
}

func findMinSize(m map[int]int) int {
	mn := math.MaxInt
	for k, _ := range m {
		mn = min(mn, k)
	}
	return mn
}

func main() {
	// --- Variables ---
	size := make(map[int]int)
	list := []int{}
	count := 0

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
		i, err := strconv.Atoi(line)
		if err != nil {
			log.Panic(err)
		}
		list = append(list, i)
	}

	// --- Combinations ---
	for i := 1; i <= len(list); i++ {
		combs := generateCombinations(list, i)
		for _, comb := range combs {
			total := sum(comb)
			if total == LITERS {
				count++
				size[len(comb)]++
			}
		}
	}

	// --- Puzzle 1 ---
	fmt.Println("1. Combinations of containers:", count)

	// --- Puzzle 2 ---
	mn := findMinSize(size)
	fmt.Println("2. Number of different ways:", size[mn])
}
