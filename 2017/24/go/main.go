package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

// --- Types ---
type Chip = [2]int

// --- Structs ---
type State struct {
	ls   []Chip
	next int
}

func (state *State) add(c Chip) {
	switch state.next {
	case c[0]:
		state.next = c[1]
	case c[1]:
		state.next = c[0]
	default:
		log.Panic("Invalid chip:", c)
	}
	state.ls = append(state.ls, c)
}

func (state *State) strength() int {
	s := 0
	for _, c := range state.ls {
		s += c[0] + c[1]
	}
	return s
}

func (state *State) clone() State {
	ls := slices.Clone(state.ls)
	next := state.next
	return State{ls, next}
}

func main() {
	// --- Helper functions ---
	parseInt := func(s string) int {
		n, err := strconv.Atoi(s)
		if err != nil {
			log.Panic(err)
		}
		return n
	}
	// --- Variables ---
	chips := []Chip{}

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
		m := strings.Split(line, "/")
		chips = append(chips, Chip{parseInt(m[0]), parseInt(m[1])})
	}

	find := func(i int) []Chip {
		ls := []Chip{}
		for _, c := range chips {
			if c[0] == i || c[1] == i {
				ls = append(ls, c)
			}
		}
		return ls
	}

	mx := -1
	longest := [][]Chip{[]Chip{}}

	queue := []State{}
	var state State
	for _, c := range find(0) {
		state = State{}
		state.add(c)
		queue = append(queue, state)
	}
	for len(queue) > 0 {
		state, queue = queue[0], queue[1:]
		for _, c := range find(state.next) {
			s := state.clone()
			if slices.Contains(s.ls, c) {
				continue
			}
			s.add(c)
			queue = append(queue, s)

			// --- Puzzle 1 ---
			mx = max(mx, s.strength())

			// --- Puzzle 2 ---
			ln0 := len(longest[0])
			ln1 := len(s.ls)
			if ln0 == ln1 {
				longest = append(longest, s.ls)
			} else if ln0 < ln1 {
				longest = [][]Chip{s.ls}
			}
		}
	}

	// --- Puzzle 1 ---
	fmt.Println("1. Max strength:", mx)

	// --- Puzzle 2 --
	mx = -1
	for _, ls := range longest {
		state = State{ls: ls, next: 0}
		mx = max(mx, state.strength())
	}
	fmt.Println("2. Max streingth:", mx)
}
