package main

import (
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

// --- Functions ---
func divisorsSum(n int) int {
	sum := 1
	for i := 2; i*i <= n; i++ {
		if n%i == 0 {
			sum += i
			if i*i != n {
				sum += n / i
			}
		}
	}
	return sum
}

func divisors(n int) []int {
	var a []int
	if n > 1 {
		a = append(a, 1, n)
	} else {
		a = append(a, 1)
	}

	sqrtN := int(math.Sqrt(float64(n)))
	for i := 2; i <= sqrtN; i++ {
		if n%i == 0 {
			a = append(a, i)
			if i*i != n {
				a = append(a, n/i)
			}
		}
	}
	return a
}

func presentsForHouse(house int) int {
	divs := divisors(house)
	var filtered []int
	for _, d := range divs {
		if d*50 >= house {
			filtered = append(filtered, d)
		}
	}
	sum := 0
	for _, x := range filtered {
		sum += x
	}
	return 11 * sum
}

func main() {
	// --- Variables ---
	house := 0
	presents := 0

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))
	input, err := strconv.Atoi(text)
	if err != nil {
		log.Panic(err)
	}

	// --- Puzzle 1 ---
	for presents < input {
		house++
		presents = 10 * divisorsSum(house)
		if house <= 2 {
			presents += 10
		} else {
			presents += 10 * (house + 1)
		}
	}
	fmt.Println("1. House number:", house)

	// --- Puzzle 2 ---
	house = 0
	presents = 0
	for presents < input {
		house++
		presents = presentsForHouse(house)
	}
	fmt.Println("2. House number:", house)
}
