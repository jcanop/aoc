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

// --- Structures ---
type Disc struct {
	ln int
	i  int
}

// Runs the simulation and finds the correct time
func simulate(discs []Disc, inits []int) int {
	time := 0
outter:
	for {
		// --- Inits discs & Skip time ---
		for i := 0; i < len(discs); i++ {
			discs[i].i = (inits[i] + time) % discs[i].ln
		}

		// --- Simulate fall ---
		for i := 0; i < len(discs); i++ {
			for j := 0; j < len(discs); j++ {
				discs[j].i = (discs[j].i + 1) % discs[j].ln
			}
			if discs[i].i != 0 { // Failed, trying again with a new time
				time++
				continue outter
			}
		}

		return time
	}
}

func main() {
	// --- Variables ---
	discs := []Disc{}
	inits := []int{}

	// --- Read input file ---
	file, err := os.Open(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// --- Parse each line ---
	re := regexp.MustCompile(`Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+)\.`)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		matches := re.FindStringSubmatch(line)
		ln, err := strconv.Atoi(matches[1])
		if err != nil {
			log.Panic(err)
		}
		i, err := strconv.Atoi(matches[2])
		if err != nil {
			log.Panic(err)
		}
		discs = append(discs, Disc{ln, 0})
		inits = append(inits, i)
	}

	// --- Puzzle 1 ---
	time := simulate(discs, inits)
	fmt.Println("1. First time you can press the button:", time)

	// --- Puzzle 2 ---
	discs = append(discs, Disc{11, 0})
	inits = append(inits, 0)
	time = simulate(discs, inits)
	fmt.Println("2. First time you can press the button:", time)
}
