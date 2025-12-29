package main

import (
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

// --- Structures ---
type point struct {
	x int
	y int
	d int
}

// Checks if two lines intersect
func intersect(x1, y1, x2, y2, x3, y3, x4, y4 int) (bool, int, int) {
	a := float64((x4-x3)*(y3-y1) - (y4-y3)*(x3-x1))
	b := float64((x4-x3)*(y2-y1) - (y4-y3)*(x2-x1))
	c := float64((x2-x1)*(y3-y1) - (y2-y1)*(x3-x1))
	if b == 0.0 { // Parallel
		return false, 0, 0
	}
	if a == b && a == 0.0 { // Coallineal
		return false, 0, 0
	}
	alfa := a / b
	beta := c / b
	if alfa >= 0 && alfa <= 1 && beta >= 0 && beta <= 1 { // Intersect
		x := float64(x1) + alfa*(float64(x2-x1))
		y := float64(y3) + beta*(float64(y4-y3))
		return true, int(math.Round(x)), int(math.Round(y))
	}
	return false, 0, 0 // Do not intersect
}

func main() {
	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))

	// --- Variables ---
	re := regexp.MustCompile("\\s*,\\s*")
	p := point{x: 0, y: 0, d: 0}
	var f *point
	list := []point{p}

	// --- Parse and follow the instructions ---
	for _, v := range re.Split(text, -1) {
		// --- Parse value ---
		r := v[0]
		i, err := strconv.Atoi(v[1:])
		if err != nil {
			log.Fatal(err)
		}

		// --- Cualculate new direction ---
		switch r {
		case 'R':
			p.d += 90
		case 'L':
			p.d -= 90
		default:
			log.Fatal("Unsupported direction:", r)
		}
		p.d %= 360

		// --- Take the steps in calculated direction ---
		switch p.d {
		case 0:
			p.y += i
		case 90, -270:
			p.x += i
		case 180, -180:
			p.y -= i
		case 270, -90:
			p.x -= i
		}

		// --- Check if it's the second time we have been in a place ---
		if f == nil {
			if len(list) >= 3 {
				b1 := &list[len(list)-1]
				b2 := &p
				for i := 0; i < len(list)-2; i++ {
					a1 := &list[i]
					a2 := &list[i+1]
					c, m, n := intersect(a1.x, a1.y, a2.x, a2.y, b1.x, b1.y, b2.x, b2.y)
					if c {
						f = &point{x: m, y: n}
						break
					}
				}
			}
			list = append(list, p)
		}
	}

	// --- Puzzle 1 ---
	distance1 := math.Abs(float64(p.x)) + math.Abs(float64(p.y))
	fmt.Println("1. Distance:", distance1)

	// --- Puzzle 2 ---
	distance2 := math.Abs(float64(f.x)) + math.Abs(float64(f.y))
	fmt.Println("2. Distance:", distance2) // < 308, > 150
}
