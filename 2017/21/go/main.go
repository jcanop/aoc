package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const START = ".#./..#/###"
const ITERATIONS_1 = 5
const ITERATIONS_2 = 18

// Creates a new grid
func newGrid(size int) [][]rune {
	grid := make([][]rune, size)
	for i := 0; i < len(grid); i++ {
		grid[i] = make([]rune, size)
	}
	return grid
}

// Creates a subgrid from a bigger grid
func subGrid(grid [][]rune, x, y, size int) [][]rune {
	ng := newGrid(size)
	for i := 0; i < size; i++ {
		for j := 0; j < size; j++ {
			ng[i][j] = grid[i+y][j+x]
		}
	}
	return ng
}

// Write a line on a grid
func writeLine(line string, grid [][]rune, x, y int) {
	r := 0
	i := 0
	for _, c := range line {
		switch c {
		case '/':
			r++
			i = 0
		default:
			grid[y+r][x+i] = c
			i++
		}
	}
}

// Gets the line that discribes the grid
func gridToLine(grid [][]rune) string {
	size := len(grid)
	line := []rune{}
	for y := 0; y < size; y++ {
		for x := 0; x < size; x++ {
			line = append(line, grid[y][x])
		}
		if y < size-1 {
			line = append(line, '/')
		}
	}
	return string(line)
}

// Rotate the grid
func rotate(grid [][]rune) [][]rune {
	size := len(grid)
	g := newGrid(size)
	for y := 0; y < size; y++ {
		for x := 0; x < size; x++ {
			g[x][size-y-1] = grid[y][x]
		}
	}
	return g
}

// Flip the grid
func flip(grid [][]rune) [][]rune {
	size := len(grid)
	g := newGrid(size)
	for y := 0; y < size; y++ {
		for x := 0; x < size; x++ {
			g[y][x] = grid[y][size-x-1]
		}
	}
	return g
}

// Find rule
func findRule(grid [][]rune, rules map[string]string) string {
	line := gridToLine(grid)
	if rule, exists := rules[line]; exists {
		return rule
	}
	for i := 0; i < 3; i++ {
		grid = rotate(grid)
		line = gridToLine(grid)
		if rule, exists := rules[line]; exists {
			return rule
		}
	}
	grid = flip(grid)
	line = gridToLine(grid)
	if rule, exists := rules[line]; exists {
		return rule
	}
	for i := 0; i < 3; i++ {
		grid = rotate(grid)
		line = gridToLine(grid)
		if rule, exists := rules[line]; exists {
			return rule
		}
	}
	return ""
}

// Count pixels that are on
func countPixels(grid [][]rune) int {
	count := 0
	for y := 0; y < len(grid); y++ {
		for x := 0; x < len(grid[0]); x++ {
			if grid[y][x] == '#' {
				count++
			}
		}
	}
	return count
}

// Prints the grind on the screen
func printGrid(grid [][]rune) {
	for y := 0; y < len(grid); y++ {
		for x := 0; x < len(grid[0]); x++ {
			fmt.Printf("%c", grid[y][x])
		}
		fmt.Println()
	}
}

func main() {
	// --- Variables ---
	rules := make(map[string]string)
	count1 := -1
	count2 := -2

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
		fs := strings.Split(line, " => ")
		rules[fs[0]] = fs[1]
	}

	// --- Initial grid ---
	grid := newGrid(3)
	writeLine(START, grid, 0, 0)

	// --- Process art ---
	for i := 0; i < ITERATIONS_2; i++ {
		size := len(grid)
		if size%2 == 0 {
			ns := size / 2 * 3
			ng := newGrid(ns)
			for j := 0; j < size/2; j++ {
				for k := 0; k < size/2; k++ {
					sg := subGrid(grid, j*2, k*2, 2)
					rule := findRule(sg, rules)
					if rule == "" {
						log.Panic("Rule not found!")
					}
					writeLine(rule, ng, j*3, k*3)
				}
			}
			grid = ng
		} else if size%3 == 0 {
			ns := size / 3 * 4
			ng := newGrid(ns)
			for j := 0; j < size/3; j++ {
				for k := 0; k < size/3; k++ {
					sg := subGrid(grid, j*3, k*3, 3)
					rule := findRule(sg, rules)
					if rule == "" {
						log.Panic("Rule not found!")
					}
					writeLine(rule, ng, j*4, k*4)
				}
			}
			grid = ng
		}

		if i == ITERATIONS_1-1 {
			count1 = countPixels(grid)
		}
	}
	count2 = countPixels(grid)

	// --- Results ---
	fmt.Println("1. Pixels on:", count1)
	fmt.Println("2. Pixels on:", count2)
}
