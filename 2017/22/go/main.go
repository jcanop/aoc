package main

import (
	"bufio"
	"fmt"
	"log"
	"maps"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const ITERATIONS_1 = 10_000
const ITERATIONS_2 = 10_000_000

func id(x, y int) string {
	return strconv.Itoa(x) + "," + strconv.Itoa(y)
}

func main() {
	// --- Variables ---
	input := make(map[string]int)

	// --- Read input file ---
	file, err := os.Open(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// --- Parse each line ---
	scanner := bufio.NewScanner(file)
	h := 0
	w := 0
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		for x, c := range line {
			if c == '#' {
				input[id(x, h)] = 2
			}
		}
		h++
		w = len(line) + 1
	}
	h++

	// --- Puzzle 1 ---
	grid := maps.Clone(input)
	x := (w - 1) / 2
	y := (h - 1) / 2
	d := 0
	count := 0
	for i := 0; i < ITERATIONS_1; i++ {
		n := id(x, y)
		if _, infected := grid[n]; infected {
			d = (d + 90) % 360
			delete(grid, n)
		} else {
			d -= 90
			if d < 0 {
				d += 360
			}
			grid[n] = 2
			count++
		}
		switch d {
		case 0:
			y--
		case 90:
			x++
		case 180:
			y++
		case 270:
			x--
		default:
			log.Panic("Unsupported dir:", d)
		}
	}
	fmt.Println("1. Bursts:", count)

	// --- Puzzle 2 ---
	grid = maps.Clone(input)
	x = (w - 1) / 2
	y = (h - 1) / 2
	d = 0
	count = 0
	for i := 0; i < 10000000; i++ {
		n := id(x, y)
		s := grid[n]
		switch s {
		case 0:
			d -= 90
			if d < 0 {
				d += 360
			}
		case 2:
			d = (d + 90) % 360
		case 3:
			d = (d + 180) % 360
		}
		s = (s + 1) % 4
		if s == 2 {
			count++
		}
		if s == 0 {
			delete(grid, n)
		} else {
			grid[n] = s
		}
		switch d {
		case 0:
			y--
		case 90:
			x++
		case 180:
			y++
		case 270:
			x--
		default:
			log.Panic("Unsupported dir:", d)
		}
	}
	fmt.Println("2. Bursts:", count) // > 2681 && < 2511973
}
