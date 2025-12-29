package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
)

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

// --- Constants ---
const FILENAME = "../input/input.txt"
const REGEX = `^(.+) to (.+) = (\d+)$`

func main() {
	// --- Variables ---
	re := regexp.MustCompile(REGEX)
	set := make(map[string]bool)
	distances := make(map[string]int)
	mn := math.MaxInt
	mx := 0

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
		from := m[1]
		to := m[2]
		dist, err := strconv.Atoi(m[3])
		if err != nil {
			log.Panic(err)
		}

		set[from] = true
		set[to] = true
		distances[from+"-"+to] = dist
		distances[to+"-"+from] = dist
	}
	places := make([]string, 0, len(set))
	for p := range set {
		places = append(places, p)
	}

	// --- Calculate distances ---
	for _, p := range permutations(places) {
		d := 0
		for i := 0; i < len(p)-1; i++ {
			d += distances[p[i]+"-"+p[i+1]]
		}
		mn = min(mn, d)
		mx = max(mx, d)
	}

	// --- Results ---
	fmt.Println("1. Min distance:", mn)
	fmt.Println("2. Max distance:", mx)
}
