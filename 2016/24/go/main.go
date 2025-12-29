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

// --- Structs ---
type Point struct {
	x int
	y int
}

// Breadth-first search
func bfs(nodes map[Point][]Point, points []Point, ini Point) []float64 {
	pm := make(map[Point]int)
	for i, p := range points {
		pm[p] = i
	}

	paths := make([]float64, len(points))
	for i := range paths {
		paths[i] = math.Inf(1)
	}

	queue := [][]interface{}{{ini, 0.0}}
	visited := make(map[Point]bool)
	visited[ini] = true
	paths[pm[ini]] = 0

	for len(queue) > 0 {
		c := queue[0][0].(Point)
		time := queue[0][1].(float64)
		queue = queue[1:]

		if i, exists := pm[c]; exists {
			paths[i] = time
		}

		done := true
		for _, dist := range paths {
			if dist == math.Inf(1) {
				done = false
				break
			}
		}

		if done {
			return paths
		}

		for _, n := range nodes[c] {
			if !visited[n] {
				queue = append(queue, []interface{}{n, time + 1})
				visited[n] = true
			}
		}
	}

	return paths
}

// Solve Puzzle 1
func solve1(paths [][]float64, remaining []int, ini int, total float64) float64 {
	if len(remaining) == 0 {
		return total
	}

	dist := math.Inf(1)
	for _, key := range remaining {
		n := make([]int, 0, len(remaining)-1)
		for _, r := range remaining {
			if r != key {
				n = append(n, r)
			}
		}
		d := solve1(paths, n, key, total+paths[ini][key])
		dist = min(d, dist)
	}

	return dist
}

// Solve Puzzle 2
func solve2(paths [][]float64, remaining []int, ini int, total float64) float64 {
	if len(remaining) == 0 {
		return total + paths[ini][0]
	}

	dist := math.Inf(1)
	for _, key := range remaining {
		n := make([]int, 0, len(remaining)-1)
		for _, r := range remaining {
			if r != key {
				n = append(n, r)
			}
		}
		d := solve2(paths, n, key, total+paths[ini][key])
		dist = min(d, dist)
	}

	return dist
}

func main() {
	// --- Variables ---
	data := []string{}
	nodes := make(map[Point][]Point)
	points := []Point{}
	width := 0
	height := 0

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
		data = append(data, line)
	}
	width = len(data[0])
	height = len(data)

	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			item := data[y]
			if item[x] == '#' {
				continue
			}
			pt := Point{x, y}
			nodes[pt] = []Point{}

			_, err := strconv.Atoi(string(item[x]))
			if err == nil {
				points = append(points, pt)
			}
		}
	}

	// --- Build the grafo ---
	deltas := [][2]int{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}
	for p := range nodes {
		for _, d := range deltas {
			x := p.x + d[0]
			y := p.y + d[1]
			n := Point{x, y}
			if _, exists := nodes[n]; exists {
				nodes[p] = append(nodes[p], n)
			}
		}
	}

	// --- Puzzle 1 ---
	paths := make([][]float64, len(points))
	for i := 0; i < len(points); i++ {
		paths[i] = bfs(nodes, points, points[i])
	}
	remaining := make([]int, 0, len(points)-1)
	for i := 1; i < len(points); i++ {
		remaining = append(remaining, i)
	}
	steps := solve1(paths, remaining, 0, 0)
	fmt.Println("1. Steps:", steps)

	// --- Puzzle 2 ---
	remaining = make([]int, 0, len(points)-1)
	for i := 1; i < len(points); i++ {
		remaining = append(remaining, i)
	}
	steps = solve2(paths, remaining, 0, 0)
	fmt.Println("2. Steps:", steps)
}
