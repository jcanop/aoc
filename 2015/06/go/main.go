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
const LEN = 1000
const REGEX = `(toggle|turn on|turn off) (\d+),(\d+) through (\d+),(\d+)`

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
	map1 := [LEN][LEN]bool{}
	map2 := [LEN][LEN]int{}

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
		op := m[1]
		x1 := parseInt(m[2])
		y1 := parseInt(m[3])
		x2 := parseInt(m[4])
		y2 := parseInt(m[5])

		for y := y1; y <= y2; y++ {
			/*
				if len(map1[y]) == 0 {
					map1[y] = [LEN][LEN]bool{}
					map2[y] = [LEN][LEN]int{}
				}
			*/
			for x := x1; x <= x2; x++ {
				switch op {
				case "toggle":
					map1[y][x] = !map1[y][x]
					map2[y][x] += 2
				case "turn on":
					map1[y][x] = true
					map2[y][x]++
				case "turn off":
					map1[y][x] = false
					if map2[y][x] > 0 {
						map2[y][x]--
					}
				default:
					log.Panic("Unsupported op:", op)
				}
			}
		}
	}

	// --- Results ---
	count1 := 0
	count2 := 0
	for y := 0; y < LEN; y++ {
		for x := 0; x < LEN; x++ {
			if map1[y][x] {
				count1++
			}
			count2 += map2[y][x]
		}
	}
	fmt.Println("1. Lights lit:", count1)
	fmt.Println("2. Total brightness:", count2)
}
