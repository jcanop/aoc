package main

import (
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
const LEN = 16
const COUNT = 1_000_000_000

// --- Structs ---
type Spin struct {
	x int
}

type Exchange struct {
	a int
	b int
}

type Partner struct {
	a rune
	b rune
}

// Simulate the full dance moves in the list
func dance(programs []rune, list []interface{}) []rune {
	ps := slices.Clone(programs)
	for _, s := range list {
		switch v := s.(type) {
		case Spin:
			ps = slices.Concat(ps[LEN-v.x:], ps[:LEN-v.x])
		case Exchange:
			ps[v.a], ps[v.b] = ps[v.b], ps[v.a]
		case Partner:
			i := slices.Index(ps, v.a)
			j := slices.Index(ps, v.b)
			ps[i], ps[j] = ps[j], ps[i]
		default:
			log.Panic("Unsupported type:", s)
		}
	}
	return ps
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
	res := regexp.MustCompile(`s(\d+)`)
	rex := regexp.MustCompile(`x(\d+)/(\d+)`)
	rep := regexp.MustCompile(`p(\w)/(\w)`)
	list := []interface{}{}
	programs := make([]rune, LEN)

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))

	// --- Parse input ---
	for _, s := range strings.Split(text, ",") {
		switch s[0] {
		case 's':
			m := res.FindStringSubmatch(s)
			list = append(list, Spin{x: parseInt(m[1])})
		case 'x':
			m := rex.FindStringSubmatch(s)
			list = append(list, Exchange{a: parseInt(m[1]), b: parseInt(m[2])})
		case 'p':
			m := rep.FindStringSubmatch(s)
			list = append(list, Partner{a: rune(m[1][0]), b: rune(m[2][0])})
		default:
			log.Panic("Unsupported move:", s)
		}
	}
	for i := 0; i < LEN; i++ {
		programs[i] = rune(i + 'a')
	}

	// --- Puzzle 1 ---
	ps := dance(programs, list)
	fmt.Println("1. Order:", string(ps))

	// --- Puzzle 2 ---
	orders := []string{string(ps)}
	for !slices.Equal(ps, programs) {
		ps = dance(ps, list)
		orders = append(orders, string(ps))
	}
	fmt.Println("2. Order:", orders[(COUNT-1)%len(orders)])
}
