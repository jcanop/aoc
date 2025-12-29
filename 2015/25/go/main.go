package main

import (
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const REGEX = `Enter the code at row (\d+), column (\d+)`
const FIRST_CODE = 20151125

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
	re := regexp.MustCompile(REGEX)

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))
	m := re.FindStringSubmatch(text)
	row := parseInt(m[1])
	column := parseInt(m[2])

	// --- Find the code ---
	n := float64(row + column - 1)
	target := int((math.Pow(n, 2)+n)/2) - (int(n) - column)
	code := FIRST_CODE
	for i := 1; i < target; i++ {
		code = (code * 252533) % 33554393
	}
	fmt.Println("Code:", code)
}
