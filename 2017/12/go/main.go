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

func main() {
	// --- Variables ---
	data := [][]int{}

	// --- Read input file ---
	file, err := os.Open(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// --- Parse each line ---
	re := regexp.MustCompile(`\s*,\s*`)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		i := strings.Index(line, " <-> ")
		ls := []int{}
		for _, s := range re.Split(line[i+5:], -1) {
			n, err := strconv.Atoi(s)
			if err != nil {
				log.Panic(err)
			}
			ls = append(ls, n)
		}
		data = append(data, ls)
	}

	// --- Search pipes ---
	sets := []map[int]struct{}{} // One set per group
outter:
	for i := 0; i < len(data); i++ {
		// --- Check is the program is in any group ---
		for _, set := range sets {
			if _, exists := set[i]; exists {
				continue outter
			}
		}

		// --- Creates a new group and looks for its members ---
		set := make(map[int]struct{})
		var search func(int)
		search = func(n int) {
			set[n] = struct{}{}
			for _, j := range data[n] {
				if _, exists := set[j]; !exists {
					search(j)
				}
			}
		}
		search(i)
		sets = append(sets, set)
	}

	// --- Results ---
	fmt.Println("1. Programs:", len(sets[0]))
	fmt.Println("2. Groups:", len(sets))
}
