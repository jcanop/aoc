package main

import (
	"bufio"
	"cmp"
	"fmt"
	"log"
	"os"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const REGEX = `^.+ can fly (\d+) km\/s for (\d+) seconds, but then must rest for (\d+) seconds\.$`
const TIME = 2503

// --- Structs ---
type Reindeer struct {
	speed     int
	fly       int
	rest      int
	distance  int
	flyCount  int
	restCount int
	points    int
}

func (r *Reindeer) tick() {
	if r.flyCount > 0 {
		r.flyCount--
		r.distance += r.speed
	} else if r.restCount > 0 {
		r.restCount--
	} else {
		r.flyCount = r.fly - 1
		r.restCount = r.rest
		r.distance += r.speed
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
	re := regexp.MustCompile(REGEX)
	ls := []Reindeer{}

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
		m := re.FindStringSubmatch(line)
		speed := parseInt(m[1])
		fly := parseInt(m[2])
		rest := parseInt(m[3])
		ls = append(ls, Reindeer{speed: speed, fly: fly, rest: rest, distance: 0, flyCount: 0, restCount: 0, points: 0})
	}

	// --- Simulate race ---
	for i := 0; i < TIME; i++ {
		for j := 0; j < len(ls); j++ {
			ls[j].tick()
		}
		mx := slices.MaxFunc(ls, func(a, b Reindeer) int {
			return cmp.Compare(a.distance, b.distance)
		}).distance
		for j := 0; j < len(ls); j++ {
			if ls[j].distance == mx {
				ls[j].points++
			}
		}
	}

	// --- Puzzle 1 ---
	mx := slices.MaxFunc(ls, func(a, b Reindeer) int {
		return cmp.Compare(a.distance, b.distance)
	}).distance
	fmt.Println("1. Winning reindeer distance:", mx)

	// --- Puzzle 2 ---
	mx = slices.MaxFunc(ls, func(a, b Reindeer) int {
		return cmp.Compare(a.points, b.points)
	}).points
	fmt.Println("2. Winning reindeer points:", mx)
}
