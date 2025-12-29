package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const REGEX = `(.+)? would (gain|lose) (\d+) happiness units by sitting next to (.+)\.`

// Calculates the permutations
func permutations(a []string) [][]string {
	var r [][]string

	var generate func(int)
	generate = func(i int) {
		if i == len(a) {
			tmp := make([]string, len(a))
			copy(tmp, a)
			r = append(r, tmp)
			return
		}
		for j := i; j < len(a); j++ {
			a[i], a[j] = a[j], a[i]
			generate(i + 1)
			a[i], a[j] = a[j], a[i]
		}
	}
	generate(0)
	return r
}

// Calculates the happiness
func calculateHappiness(persons []string, happines map[string]int) int {
	mx := math.MinInt
	for _, p := range permutations(persons) {
		h := 0
		for i := 0; i < len(p); i++ {
			n1 := p[i]
			n2 := p[(i+1)%len(p)]
			h += happines[n1+"-"+n2]
			h += happines[n2+"-"+n1]
		}
		mx = max(mx, h)
	}
	return mx
}

func main() {
	// --- Variables ---
	re := regexp.MustCompile(REGEX)
	persons := []string{}
	happiness := make(map[string]int)

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
		if !slices.Contains(persons, m[1]) {
			persons = append(persons, m[1])
		}
		if !slices.Contains(persons, m[4]) {
			persons = append(persons, m[4])
		}
		h, err := strconv.Atoi(m[3])
		if err != nil {
			log.Panic(err)
		}
		if m[2] == "lose" {
			h *= -1
		}
		happiness[m[1]+"-"+m[4]] = h
	}

	// --- Puzzle 1 ---
	mx := calculateHappiness(persons, happiness)
	fmt.Println("1. Max change in happiness:", mx)

	// --- Puzzle 2 ---
	persons = append(persons, "me")
	for _, p := range persons {
		happiness[p+"-me"] = 0
		happiness["me-"+p] = 0
	}
	mx = calculateHappiness(persons, happiness)
	fmt.Println("2. Max change in happiness:", mx)
}
