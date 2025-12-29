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

// --- Structs ---
type Robot struct {
	chips  [2]int
	lower  int // Positive numbers = robot, negative numbers = output
	higher int
}

func (r *Robot) addChip(value int) {
	if r.chips[0] == -1 {
		r.chips[0] = value
	} else if r.chips[1] == -1 {
		r.chips[1] = value
	} else {
		log.Panic("Overflow!")
	}
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
	re1 := regexp.MustCompile(`value (\d+) goes to bot (\d+)`)
	re2 := regexp.MustCompile(`bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)`)
	robots := make(map[int]*Robot)
	outputs := make(map[int]int)
	result1 := -1

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
		if strings.HasPrefix(line, "value") {
			matches := re1.FindStringSubmatch(line)
			v := parseInt(matches[1])
			b := parseInt(matches[2])
			r := robots[b]
			if r == nil {
				r = &Robot{chips: [2]int{-1, -1}}
				robots[b] = r
			}
			r.addChip(v)
		} else if strings.HasPrefix(line, "bot") {
			matches := re2.FindStringSubmatch(line)
			b := parseInt(matches[1])
			l := parseInt(matches[3])
			h := parseInt(matches[5])
			if matches[2] == "output" {
				l = -1*l - 1
			}
			if matches[4] == "output" {
				h = -1*h - 1
			}
			r := robots[b]
			if r == nil {
				r = &Robot{chips: [2]int{-1, -1}}
				robots[b] = r
			}
			r.lower = l
			r.higher = h
		}
	}

	// --- Runs the simulation ---
	for {
		found := false
		for b, r := range robots {
			if r.chips[0] != -1 && r.chips[1] != -1 {
				mx := max(r.chips[0], r.chips[1])
				mn := min(r.chips[0], r.chips[1])
				// --- Puzzle 1 ---
				if mx == 61 && mn == 17 {
					result1 = b
				}
				if r.lower >= 0 {
					rl := robots[r.lower]
					if rl == nil {
						rl = &Robot{chips: [2]int{-1, -1}}
						robots[r.lower] = rl
					}
					rl.addChip(mn)
				} else {
					outputs[-1*r.lower-1] = mn
				}
				if r.higher >= 0 {
					rh := robots[r.higher]
					if rh == nil {
						rh = &Robot{chips: [2]int{-1, -1}}
						robots[r.higher] = rh
					}
					rh.addChip(mx)
				} else {
					outputs[-1*r.higher-1] = mx
				}
				r.chips[0] = -1
				r.chips[1] = -1
				found = true
			}
		}
		if !found {
			break
		}
	}

	// --- Puzzle 2 ---
	result2 := 1
	for i := 0; i < 3; i++ {
		result2 *= outputs[i]
	}

	// --- Results ---
	fmt.Println("1. Bot:", result1)
	fmt.Println("2. Result:", result2) // > 413
}
