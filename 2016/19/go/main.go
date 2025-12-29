package main

import (
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
	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	line := strings.TrimSpace(string(bytes))
	count, err := strconv.Atoi(line)
	if err != nil {
		log.Panic(err)
	}

	// --- Puzzle 1 ---
	b := strconv.FormatUint(uint64(count), 2)
	r := b[1:] + b[0:1]
	elf, _ := strconv.ParseUint(r, 2, 64)
	fmt.Println("1. Elf with all the presents:", elf)

	// --- Puzzle 2 ---
	logN := math.Log(float64(count))
	p := math.Pow(3, math.Floor(logN/math.Log(3)))
	i := int(p)
	if count == i {
		elf = uint64(count)
	} else {
		elf = uint64(count - i + int(math.Max(0, float64(count-2*i))))
	}
	fmt.Println("2. Elf with all the presents:", elf)
}
