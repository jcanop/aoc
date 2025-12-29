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

func main() {
	// --- Variables ---
	grid := []string{}

	// --- Read input file ---
	file, err := os.Open(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// --- Parse each line ---
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if len(strings.TrimSpace(line)) > 0 {
			grid = append(grid, line)
		}
	}
	h := len(grid)
	w := len(grid[0])

	// --- Follow the path ---
	msg := ""
	count := 0
	var x, y int
	var dir rune = 'D'

	for grid[y][x] != '|' {
		x++
	}
	for {
		c := grid[y][x]
		count++
		if c == '+' {
			if y > 0 && grid[y-1][x] != ' ' && dir != 'D' {
				dir = 'U'
				y--
			} else if y < h-1 && grid[y+1][x] != ' ' && dir != 'U' {
				dir = 'D'
				y++
			} else if x > 0 && grid[y][x-1] != ' ' && dir != 'R' {
				dir = 'L'
				x--
			} else if x < w-1 && grid[y][x+1] != ' ' && dir != 'L' {
				dir = 'R'
				x++
			} else {
				log.Panic("I don't know where to go!")
			}
		} else {
			if c != '|' && c != '-' {
				msg += string(c)
			}
			switch dir {
			case 'U':
				y--
			case 'D':
				y++
			case 'L':
				x--
			case 'R':
				x++
			}
			if x < 0 || x >= w || y < 0 || y >= h || grid[y][x] == ' ' {
				break
			}
		}
	}

	// --- Results ---
	fmt.Println("1. Letters:", msg)
	fmt.Println("2. Steps:", count)
}
