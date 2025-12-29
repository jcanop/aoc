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

func main() {
	// --- Variables ---
	list1 := [3]int{}
	count1 := 0
	list2 := [3][3]int{}
	count2 := 0
	ln := 0

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
		fields := strings.Fields(line)
		for i, v := range fields {
			n, err := strconv.Atoi(v)
			if err != nil {
				log.Fatal(err)
			}
			list1[i] = n
			list2[i][ln] = n
		}
		sort.Ints(list1[:])
		if list1[0]+list1[1] > list1[2] {
			count1++
		}
		if ln == 2 {
			for i := 0; i < 3; i++ {
				sort.Ints(list2[i][:])
				if list2[i][0]+list2[i][1] > list2[i][2] {
					count2++
				}
			}
		}
		ln = (ln + 1) % 3
	}

	fmt.Println("1. Possible trianges:", count1)
	fmt.Println("2. Possible trianges:", count2)
}
