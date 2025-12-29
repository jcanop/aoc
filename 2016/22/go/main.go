package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const ID_MASK = 1000

// --- Structs ---
type Node struct {
	x     int
	y     int
	size  int
	used  int
	avail int
}

// Create the node ID
func id(x, y int) int {
	return x*ID_MASK + y
}

// Find the empty node
func findEmpty(nodes []Node) *Node {
	for _, r := range nodes {
		if r.used == 0 {
			return &r
		}
	}
	return nil
}

// Find the max X and Y
func findMax(nodes []Node) (int, int) {
	x := 0
	y := 0
	for _, r := range nodes {
		x = max(x, r.x)
		y = max(y, r.y)
	}
	return x, y
}

// Find the path
func findPath(nodes []Node, index map[int]*Node) int {
	empty := findEmpty(nodes)
	n := empty
	queue := []*Node{n}
	visited := make(map[int]int)
	visited[id(n.x, n.y)] = 0

	mx, my := findMax(nodes)
	final := id(mx-1, 0)
	for {
		if len(queue) == 0 {
			return -1
		}

		n, queue = queue[0], queue[1:]
		nid := id(n.x, n.y)
		if nid == final {
			return visited[nid] + 5*(mx-1) + 1
		}

		for dx := -1; dx <= 1; dx++ {
			for dy := -1; dy <= 1; dy++ {
				if (dx == dy) || (dx == -1 && dy == 1) || (dx == 1 && dy == -1) ||
					(n.x == 0 && dx == -1) || (n.x == mx && dx == 1) ||
					(n.y == 0 && dy == -1) || (n.y == my && dy == 1) {
					continue
				}

				mid := id(n.x+dx, n.y+dy)
				_, exists := visited[mid]
				if exists {
					continue
				}
				m := index[mid]
				if m.used > empty.avail {
					continue
				}
				queue = append(queue, m)
				visited[mid] = visited[nid] + 1
			}
		}
	}

	return 0
}

func main() {
	parseInt := func(s string) int {
		n, err := strconv.Atoi(s)
		if err != nil {
			log.Panic(err)
		}
		return n
	}

	// --- Variables ---
	re := regexp.MustCompile(`node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T`)
	nodes := []Node{}
	index := make(map[int]*Node)

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
		if line[0] == '/' {
			m := re.FindStringSubmatch(line)
			x := parseInt(m[1])
			y := parseInt(m[2])
			size := parseInt(m[3])
			used := parseInt(m[4])
			avail := parseInt(m[5])
			nodes = append(nodes, Node{x, y, size, used, avail})
			index[id(x, y)] = &nodes[len(nodes)-1]
		}
	}

	// --- Puzzle 1 ---
	count := 0
	for i := 0; i < len(nodes); i++ {
		for j := 0; j < len(nodes); j++ {
			if i != j && nodes[i].used > 0 && nodes[i].used <= nodes[j].avail {
				count++
			}
		}
	}
	fmt.Println("1. Viables pairs:", count)

	// --- Puzzle 2 ---
	steps := findPath(nodes, index)
	fmt.Println("2. Fewest number of steps:", steps) // > 197
}
