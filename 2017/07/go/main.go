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

const REGEX = `(\w+) \((\d+)\)( -> .+)?`

// --- Structs ---
type Program struct {
	name   string
	weight int
	sub    []string
}

func main() {
	// --- Variables ---
	re := regexp.MustCompile(REGEX)
	programs := []Program{}
	index := make(map[string]*Program)
	var root *Program
	var weight int

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
		name := m[1]
		weight, err := strconv.Atoi(m[2])
		if err != nil {
			log.Panic(err)
		}
		sub := []string{}
		if len(m[3]) > 0 {
			for _, v := range strings.Split(m[3][4:], ",") {
				sub = append(sub, strings.TrimSpace(v))
			}
		}
		p := Program{name, weight, sub}
		programs = append(programs, p)
		index[p.name] = &p
	}

	// --- Puzzle 1 ---
outter:
	for i := 0; i < len(programs); i++ {
		for j := 0; j < len(programs); j++ {
			if i == j {
				continue
			}
			if slices.Contains(programs[j].sub, programs[i].name) {
				continue outter
			}
		}
		root = &programs[i]
		break
	}

	// --- Puzzle 2 ---
	var check func(string) int
	check = func(name string) int {
		subs := index[name].sub
		weights := make([]int, len(subs))
		for i, s := range subs {
			weights[i] = check(s)
		}

		counts := make(map[int]int)
		for _, w := range weights {
			counts[w]++
		}

		if len(counts) == 2 {
			var correct, wrong int
			mx := -1
			for v, c := range counts {
				if c > mx {
					mx = c
					correct = v
				}
			}
			for v := range counts {
				if v != correct {
					wrong = v
					break
				}
			}
			diff := correct - wrong

			node := ""
			for i, w := range weights {
				if w == wrong {
					node = subs[i]
					break
				}
			}
			if node != "" {
				weight = index[node].weight + diff
			}

			total := 0
			for _, w := range weights {
				total += w
			}
			return index[name].weight + total + diff
		}

		total := 0
		for _, w := range weights {
			total += w
		}
		return index[name].weight + total
	}
	check(root.name)

	// --- Results ---
	fmt.Println("1. Bottom program:", root.name)
	fmt.Println("2. Weight:", weight)
}
