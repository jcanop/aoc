package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const REGEX = `(-?\d+)`

func sum(jsonValue interface{}) int {
	switch v := jsonValue.(type) {
	case float64:
		return int(v)
	case []interface{}:
		total := 0
		for _, x := range v {
			total += sum(x)
		}
		return total
	case map[string]interface{}:
		for _, x := range v {
			if str, ok := x.(string); ok && str == "red" {
				return 0
			}
		}
		total := 0
		for _, x := range v {
			total += sum(x)
		}
		return total
	default:
		return 0
	}
}

func main() {
	// --- Variables ---
	re := regexp.MustCompile(REGEX)

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))

	// --- Puzzle 1 ---
	total := 0
	for _, c := range re.FindAllString(text, -1) {
		n, err := strconv.Atoi(c)
		if err != nil {
			log.Panic(err)
		}
		total += n
	}
	fmt.Println("1. Sum of all numbers:", total)

	// --- Puzzle 2 ---
	var data interface{}
	err = json.Unmarshal([]byte(text), &data)
	if err != nil {
		log.Panic(err)
	}
	total = sum(data)
	fmt.Println("2.Sum of all numbers:", total)
}
