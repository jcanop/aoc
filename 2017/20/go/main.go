package main

import (
	"bufio"
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
const REGEX = `p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>`

// --- Structs ---
type Particule struct {
	p, v, a [3]int
}

func (p *Particule) position() int {
	return abs(p.p[0]) + abs(p.p[1]) + abs(p.p[2])
}

func (p *Particule) velocity() int {
	return abs(p.v[0]) + abs(p.v[1]) + abs(p.v[2])
}

func (p *Particule) acceleration() int {
	return abs(p.a[0]) + abs(p.a[1]) + abs(p.a[2])
}

func abs(n int) int {
	if n < 0 {
		return -1 * n
	}
	return n
}

func main() {
	// --- Helper Functions ---
	parseInt := func(s string) int {
		n, err := strconv.Atoi(s)
		if err != nil {
			log.Panic(err)
		}
		return n
	}

	// --- Variables ---
	list := []Particule{}
	closest := 0

	// --- Read input file ---
	file, err := os.Open(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// --- Parse each line ---
	re := regexp.MustCompile(REGEX)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		m := re.FindStringSubmatch(line)
		p := [3]int{parseInt(m[1]), parseInt(m[2]), parseInt(m[3])}
		v := [3]int{parseInt(m[4]), parseInt(m[5]), parseInt(m[6])}
		a := [3]int{parseInt(m[7]), parseInt(m[8]), parseInt(m[9])}
		list = append(list, Particule{p, v, a})

		// --- Puzzle 1 ---
		p1 := &list[closest]
		p2 := &list[len(list)-1]
		a1 := p1.acceleration()
		a2 := p2.acceleration()
		if a1 > a2 {
			closest = len(list) - 1
		} else if a1 == a2 {
			pos1 := p1.position()
			pos2 := p2.position()
			if pos1 > pos2 || (pos1 == pos2 && p1.velocity() > p2.velocity()) {
				closest = len(list) - 1
			}
		}
	}

	// --- Puzzle 2 ---
	for i := 0; i < 1000; i++ {
		for j := 0; j < len(list); j++ {
			p := &list[j]
			for k := 0; k < 3; k++ {
				p.v[k] += p.a[k]
				p.p[k] += p.v[k]
			}
		}
		// --- Find collisions ---
		set := []int{}
		for j := 0; j < len(list); j++ {
			for k := j + 1; k < len(list); k++ {
				p1 := &list[j]
				p2 := &list[k]
				if p1.p == p2.p {
					set = append(set, j, k)
				}
			}
		}
		// --- Remove collided particles from the list ---
		slices.Sort(set)
		set = slices.Compact(set)
		for j := len(set) - 1; j >= 0; j-- {
			list = slices.Delete(list, set[j], set[j]+1)
		}
	}

	// --- Results ---
	fmt.Println("1. Closest to position <0,0,0>:", closest)
	fmt.Println("2. Particules left:", len(list))
}
