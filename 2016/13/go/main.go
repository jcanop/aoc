package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const START_X = 1
const START_Y = 1
const FINISH_X = 31
const FINISH_Y = 39

// --- Structs ---
type Point struct {
	x     int
	y     int
	steps int
}

func isOpen(seed, x, y uint64) bool {
	n := x*x + 3*x + 2*x*y + y + y*y + seed
	c := 0
	for i := 0; i < 64; i++ {
		if n&0x1 == 0x1 {
			c++
		}
		n = n >> 1
	}
	return c%2 == 0
}

func main() {
	// --- Variables ---
	visited := make(map[string]struct{})
	queue := []Point{}
	steps := -1
	locations := -1

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))
	seed, err := strconv.ParseUint(text, 10, 64)
	if err != nil {
		log.Panic("Invalid seed:", err)
	}

	// --- Search for the shortest path ---
	p := Point{x: START_X, y: START_Y, steps: 0}
	visited[fmt.Sprint(p.x, ",", p.y)] = struct{}{}
	queue = append(queue, p)
	for {
		if len(queue) == 0 {
			log.Panic("Path not found!")
		}

		p, queue = queue[0], queue[1:]
		if steps == -1 && p.x == FINISH_X && p.y == FINISH_Y {
			steps = p.steps
		}
		if locations == -1 && p.steps == 50 {
			locations = len(visited)
		}
		if steps != -1 && locations != -1 {
			break
		}

		for j := -1; j <= 1; j++ {
			for i := -1; i <= 1; i++ {
				if (i == 0 && j == 0) || (i == -1 && j == -1) || (i == -1 && j == 1) || (i == 1 && j == -1) || (i == 1 && j == 1) || p.x+i < 0 || p.y+j < 0 {
					continue
				}
				if isOpen(seed, uint64(p.x+i), uint64(p.y+j)) {
					k := fmt.Sprint(p.x+i, ",", p.y+j)
					_, exists := visited[k]
					if !exists {
						visited[k] = struct{}{}
						queue = append(queue, Point{x: p.x + i, y: p.y + j, steps: p.steps + 1})
					}
				}
			}
		}
	}

	// --- Print results ---
	fmt.Println("1. Fewest number of steps:", steps)
	fmt.Println("2. Number of locations:", locations)
}
