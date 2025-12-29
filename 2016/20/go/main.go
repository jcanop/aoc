package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

// --- Structs ---
type Range struct {
	ini uint32
	end uint32
}

// Fixes the firewall list
func fix(list []Range) []Range {
	sort.Slice(list, func(i, j int) bool {
		return list[i].ini < list[j].ini
	})

	i := 0
	ls := []Range{list[0]}
	for _, r := range list[1:] {
		if ls[i].ini <= r.ini && ls[i].end >= r.end {
			continue
		}
		if ls[i].ini <= r.ini && ls[i].end >= r.ini-1 && ls[i].end < r.end {
			ls[i].end = r.end
		} else if ls[i].end <= r.ini {
			ls = append(ls, r)
			i++
		}
	}
	return ls
}

// Count allowed IPs
func countAllowed(list []Range) int {
	count := 0
	for i := 0; i < len(list)-1; i++ {
		count += int(list[i+1].ini - list[i].end - 1)
	}
	return count
}

func main() {
	// --- Variables ---
	list := []Range{}

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
		tokens := strings.Split(line, "-")
		ini, err := strconv.ParseUint(tokens[0], 10, 32)
		if err != nil {
			log.Panic(err)
		}
		end, err := strconv.ParseUint(tokens[1], 10, 32)
		if err != nil {
			log.Panic(err)
		}
		list = append(list, Range{uint32(ini), uint32(end)})
	}

	// --- Fix the poorly maintained firewall list ---
	list = fix(list)

	// --- Puzzle 1 ---
	ip := list[0].end + 1
	fmt.Println("1. Lowest-valued IP:", ip)

	// --- Puzzle 2 ---
	count := countAllowed(list)
	fmt.Println("2. Allowed IPs:", count)
}
