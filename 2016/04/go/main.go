package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const ROOM = "northpole object storage"

// --- Structs ---
type item struct {
	key   rune
	count int
}

// Calcualates the checksum of a string
func checksum(value string) string {
	m := make(map[rune]int)
	for _, c := range value {
		if c != '-' {
			m[c]++
		}
	}
	s := []item{}
	for k, c := range m {
		s = append(s, item{key: k, count: c})
	}
	sort.Slice(s, func(i, j int) bool {
		if s[i].count == s[j].count {
			return s[i].key < s[j].key
		}
		return s[i].count > s[j].count
	})
	r := ""
	for _, i := range s[:5] {
		r += string(i.key)
	}
	return r
}

// Decrypts a string using shift cipher
func decrypt(value string, shift int) string {
	i := rune(shift % ('z' - 'a' + 1))
	result := ""
	for _, c := range value {
		if c == '-' {
			result += " "
		} else {
			result += string((c-'a'+i)%('z'-'a'+1) + 'a')
		}
	}
	return result
}

func main() {
	// --- Variables ---
	re := regexp.MustCompile(`([a-z\-]+)\-(\d+)\[([a-z]+)\]`)
	result1 := 0
	result2 := -1

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
		matches := re.FindStringSubmatch(line)
		if matches == nil {
			log.Fatal("No match found!", line)
		}
		name := matches[1]
		id, err := strconv.Atoi(matches[2])
		if err != nil {
			log.Fatal(err)
		}
		sum := matches[3]

		// --- Puzzle 1 ---
		if sum == checksum(name) {
			result1 += id
		}

		// --- Puzzle 2 ---
		if result2 == -1 && decrypt(name, id) == ROOM {
			result2 = id
		}
	}

	// --- Results ---
	fmt.Println("1. Sum of the sector IDs:", result1)
	fmt.Println("2. Sector ID:", result2)
}
