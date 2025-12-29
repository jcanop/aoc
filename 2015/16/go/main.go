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
const REGEX = `([a-z]+): (\d+)`

// --- MFCSAM Output ---
var MAP = map[string]int{
	"children":    3,
	"cats":        7,
	"samoyeds":    2,
	"pomeranians": 3,
	"akitas":      0,
	"vizslas":     0,
	"goldfish":    5,
	"trees":       3,
	"cars":        2,
	"perfumes":    1,
}

// --- Functions ---
func find1(line string, re *regexp.Regexp) bool {
	ms := re.FindAllStringSubmatch(line, -1)
	for _, m := range ms {
		key := m[1]
		val, err := strconv.Atoi(m[2])
		if err != nil {
			log.Panic(err)
		}
		if v, ok := MAP[key]; ok && v != val {
			return false
		}
	}
	return true
}

func find2(line string, re *regexp.Regexp) bool {
	ms := re.FindAllStringSubmatch(line, -1)
	for _, m := range ms {
		key := m[1]
		val, err := strconv.Atoi(m[2])
		if err != nil {
			log.Panic(err)
		}
		if v, ok := MAP[key]; ok {
			switch key {
			case "cats", "trees":
				if v >= val {
					return false
				}
			case "pomeranians", "goldfish":
				if v <= val {
					return false
				}
			default:
				if v != val {
					return false
				}
			}
		}
	}
	return true
}

func main() {
	// --- Variables ---
	re := regexp.MustCompile(REGEX)
	sue1 := ""
	sue2 := ""

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

		// --- Puzzle 1 ---
		if sue1 == "" && find1(line, re) {
			parts := strings.SplitN(line, ":", 2)
			if len(parts) > 0 {
				sue1 = strings.TrimSpace(parts[0])
			}
		}

		// --- Puzzle 2 ---
		if sue2 == "" && find2(line, re) {
			parts := strings.SplitN(line, ":", 2)
			if len(parts) > 0 {
				sue2 = strings.TrimSpace(parts[0])
			}
		}
	}

	// --- Results ---
	fmt.Println("1. Number of the Sue that got you the gift:", sue1)
	fmt.Println("2. Number of the Sue that got you the gift:", sue2)
}
