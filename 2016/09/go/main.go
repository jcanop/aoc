package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

// Calculates the size of the decompressed string using version 1
func decompressedSizeV1(s string) int {
	result := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '(' {
			j := strings.Index(s[i+1:], "x") + i + 1
			k := strings.Index(s[i+1:], ")") + i + 1
			n, err := strconv.Atoi(s[i+1 : j])
			if err != nil {
				log.Panic(err)
			}
			m, err := strconv.Atoi(s[j+1 : k])
			if err != nil {
				log.Panic(err)
			}
			result += m * n
			i = k + n
		} else {
			result++
		}
	}
	return result
}

// Calculates the size of the decompressed string using version 2
func decompressedSizeV2(s string) int64 {
	if len(s) == 0 {
		return 0
	}
	if s[0] == '(' {
		i := strings.Index(s, "x")
		j := strings.Index(s, ")")
		n, err := strconv.Atoi(s[1:i])
		if err != nil {
			log.Panic(err)
		}
		m, err := strconv.Atoi(s[i+1 : j])
		if err != nil {
			log.Panic(err)
		}
		s1 := s[j+1 : j+n+1]
		s2 := s[j+n+1:]
		return int64(m)*decompressedSizeV2(s1) + decompressedSizeV2(s2)
	}
	return int64(1) + decompressedSizeV2(s[1:])
}

func main() {
	// --- Variables ---
	size1 := 0
	size2 := int64(0)

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
		size1 += decompressedSizeV1(line)

		// --- Puzzle 2 ---
		size2 += decompressedSizeV2(line)
	}

	// --- Results ---
	fmt.Println("1. Decompressed length:", size1)
	fmt.Println("2. Decompressed length:", size2)
}
