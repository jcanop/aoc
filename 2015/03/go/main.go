package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

// --- Structs ---
type Point struct {
	x int
	y int
}

func main() {
	// --- Helper functions ---
	id := func(x, y int) string {
		return fmt.Sprintf("%d,%d", x, y)
	}

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))

	// --- Parse data ---
	p := Point{}
	s := Point{}
	r := Point{}
	set1 := make(map[string]struct{})
	set2 := make(map[string]struct{})
	set1[id(0, 0)] = struct{}{}
	set2[id(0, 0)] = struct{}{}
	n := 0
	for _, c := range text {
		n++
		if c == '^' {
			p.y--
			if n%2 == 0 {
				r.y--
			} else {
				s.y--
			}
		}
		if c == 'v' {
			p.y++
			if n%2 == 0 {
				r.y++
			} else {
				s.y++
			}
		}
		if c == '<' {
			p.x--
			if n%2 == 0 {
				r.x--
			} else {
				s.x--
			}
		}
		if c == '>' {
			p.x++
			if n%2 == 0 {
				r.x++
			} else {
				s.x++
			}
		}
		set1[id(p.x, p.y)] = struct{}{}
		if n%2 == 0 {
			set2[id(r.x, r.y)] = struct{}{}
		} else {
			set2[id(s.x, s.y)] = struct{}{}
		}
	}

	// --- Results ---
	fmt.Println("1. Houses with at least one present:", len(set1))
	fmt.Println("2. Houses with at least one present:", len(set2))
}
