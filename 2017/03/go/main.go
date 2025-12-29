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

// --- Structs ---
type Point struct {
	x, y int
}

func position(index int) Point {
	r := int(math.Sqrt(float64(index-1))+1) / 2
	d := 2*r - 1
	i := index - d*d - 1

	switch {
	case i < d:
		return Point{r, i - r + 1}
	case i < 2*d+2:
		return Point{r - i + d, r}
	case i < 3*d+2:
		return Point{-r, r - i - 1 + 2*d + 2}
	default:
		return Point{i - r - 3*d - 2, -r}
	}
}

func main() {
	// --- Helper functions ---
	abs := func(x int) int {
		if x < 0 {
			return -x
		}
		return x
	}

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
	p := position(input)
	steps := abs(p.x) + abs(p.y)
	fmt.Println("1. Steps:", steps)

	// --- Puzzle 2 ---
	grid := map[Point]int{
		{0, 0}: 1,
	}

	neighbors := []Point{
		{-1, -1}, {0, -1}, {1, -1},
		{-1, 0}, {1, 0},
		{-1, 1}, {0, 1}, {1, 1},
	}

	value := 1
	index := 2
	for value <= input {
		p = position(index)
		index++

		sum := 0
		for _, n := range neighbors {
			sum += grid[Point{p.x + n.x, p.y + n.y}]
		}

		value = sum
		grid[p] = value
	}
	fmt.Println("2. First value written:", value)
}
