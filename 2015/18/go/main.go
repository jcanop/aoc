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
const STEPS = 100
const ON = '#'
const OFF = '.'

func countNeighbors(grid [][]rune, x, y int) int {
	c := 0
	ylen := len(grid)
	xlen := len(grid[0])

	for j := -1; j <= 1; j++ {
		ny := y + j
		if ny < 0 || ny >= ylen {
			continue
		}
		for i := -1; i <= 1; i++ {
			nx := x + i
			if i == 0 && j == 0 {
				continue
			}
			if nx < 0 || nx >= xlen {
				continue
			}
			if ny < 0 || ny >= ylen {
				continue
			}
			if grid[ny][nx] == ON {
				c++
			}
		}
	}
	return c
}

func animation(data [][]rune, puzzle int) int {
	grid1 := make([][]rune, len(data))
	grid2 := make([][]rune, len(data))

	for r := range data {
		row1 := make([]rune, len(data[r]))
		copy(row1, data[r])
		row2 := make([]rune, len(data[r]))
		copy(row2, data[r])
		grid1[r] = row1
		grid2[r] = row2
	}
	xlen := len(data[0])
	ylen := len(data)

	grid := grid1
	buffer := grid2

	if puzzle == 2 {
		grid[0][0] = ON
		grid[0][xlen-1] = ON
		grid[ylen-1][0] = ON
		grid[ylen-1][xlen-1] = ON
	}

	for step := 0; step < STEPS; step++ {
		for y := 0; y < ylen; y++ {
			for x := 0; x < xlen; x++ {
				if puzzle == 2 {
					if (x == 0 && y == 0) || (x == 0 && y == ylen-1) || (x == xlen-1 && y == 0) || (x == xlen-1 && y == ylen-1) {
						buffer[y][x] = ON
						continue
					}
				}
				c := countNeighbors(grid, x, y)
				if grid[y][x] == ON {
					if c == 2 || c == 3 {
						buffer[y][x] = ON
					} else {
						buffer[y][x] = OFF
					}
				} else {
					if c == 3 {
						buffer[y][x] = ON
					} else {
						buffer[y][x] = OFF
					}
				}
			}
		}

		if step%2 == 0 {
			grid, buffer = grid2, grid1
		} else {
			grid, buffer = grid1, grid2
		}
	}

	total := 0
	for y := 0; y < ylen; y++ {
		for x := 0; x < xlen; x++ {
			if grid[y][x] == ON {
				total++
			}
		}
	}
	return total
}

func main() {
	// --- Variables ---
	data := [][]rune{}

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
		row := []rune(line)
		data = append(data, row)
	}

	// --- Puzzle 1 ---
	c := animation(data, 1)
	fmt.Println("1. Number of lights on:", c)

	// --- Puzzle 2 ---
	c = animation(data, 2)
	fmt.Println("2. Number of lights on:", c)
}
