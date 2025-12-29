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

// --- Constants ---
const FILENAME = "../input/input.txt"
const REGEX = `^[a-zA-Z]+: capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)$`
const MAX = 100

// --- Structs ---
type Ingredient struct {
	capacity   int
	durability int
	flavor     int
	texture    int
	calories   int
}

func main() {
	// --- Helper functions ---
	parseInt := func(s string) int {
		n, err := strconv.Atoi(s)
		if err != nil {
			log.Panic(err)
		}
		return n
	}

	notNegative := func(v int) int {
		if v < 0 {
			return 0
		}
		return v
	}

	// --- Variables ---
	re := regexp.MustCompile(REGEX)
	ingredients := []Ingredient{}
	max1 := math.MinInt
	max2 := math.MinInt

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
		capacity := parseInt(m[1])
		durability := parseInt(m[2])
		flavor := parseInt(m[3])
		texture := parseInt(m[4])
		calories := parseInt(m[5])
		ingredients = append(ingredients, Ingredient{capacity, durability, flavor, texture, calories})
	}

	// --- Calculate combinations ---
	list := make([]int, len(ingredients)-1)
	list = append(list, MAX)

	last := len(list) - 1
outer:
	for list[0] < MAX {
		capacity := 0
		durability := 0
		flavor := 0
		texture := 0
		calories := 0
		for i := 0; i < len(ingredients); i++ {
			capacity += list[i] * ingredients[i].capacity
			durability += list[i] * ingredients[i].durability
			flavor += list[i] * ingredients[i].flavor
			texture += list[i] * ingredients[i].texture
			calories += list[i] * ingredients[i].calories
		}
		capacity = notNegative(capacity)
		durability = notNegative(durability)
		flavor = notNegative(flavor)
		texture = notNegative(texture)
		calories = notNegative(calories)
		total := capacity * durability * flavor * texture
		max1 = max(max1, total)
		if calories == 500 {
			max2 = max(max2, total)
		}

		// --- Calculate next recipe ---
		i := last - 1
		carry := true
		for carry {
			limit := MAX
			if i > 0 {
				for j := 0; j < i; j++ {
					limit -= list[i]
				}
			}
			list[i]++
			if list[i] > limit {
				list[i] = 0
				if i > 0 {
					i--
				} else {
					break outer
				}
			} else {
				carry = false
			}
			list[last] = MAX
			for j := 0; j < last; j++ {
				list[last] -= list[j]
			}
		}
	}

	// --- Puzzle 1 ---
	fmt.Println("1. Total score of the highest-scoring cookie:", max1)

	// --- Puzzle 2 ---
	fmt.Println("2. Total score of the highest-scoring cookie:", max2)
}
