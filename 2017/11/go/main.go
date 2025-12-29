package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

func main() {
	// --- Helper functions ---
	abs := func(a int) int {
		if a < 0 {
			return -1 * a
		}
		return a
	}
	dist := func(x, y, z int) int {
		return (abs(x) + abs(y) + abs(z)) / 2
	}

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))

	// --- Simulate movement ---
	var x, y, z, md int
	for _, d := range strings.Split(text, ",") {
		switch d {
		case "n":
			y++
			z++
		case "ne":
			x++
			y++
		case "se":
			x++
			z--
		case "s":
			y--
			z--
		case "sw":
			x--
			y--
		case "nw":
			x--
			z++
		default:
			log.Panic("Invalid direction:", d)

		}
		md = max(md, dist(x, y, z))
	}

	// --- Results ---
	fmt.Println("1. Steps:", dist(x, y, z))
	fmt.Println("2. Furthest:", md)
}
