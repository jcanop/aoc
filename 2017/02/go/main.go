package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

func main() {
	// --- Variables ---
	data := [][]int{}
	checksum := 0
	sum := 0

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
		row := []int{}
		for _, c := range strings.Fields(line) {
			n, err := strconv.Atoi(c)
			if err != nil {
				log.Panic(err)
			}
			row = append(row, n)
		}
		data = append(data, row)
	}

	// --- Puzzle 1 ---
	for _, row := range data {
		mn := math.MaxInt
		mx := math.MinInt
		for _, n := range row {
			mn = min(mn, n)
			mx = max(mx, n)
		}
		checksum += (mx - mn)
	}
	fmt.Println("1. Checksum:", checksum)

	// --- Puzzle 2 ---
	for _, row := range data {
	outter:
		for i := 0; i < len(row); i++ {
			for j := 0; j < len(row); j++ {
				if i == j {
					continue
				}
				if row[i]%row[j] == 0 {
					sum += (row[i] / row[j])
					break outter
				}
			}
		}
	}
	fmt.Println("2. Sum:", sum)
}
