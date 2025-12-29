package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

func main() {
	// --- Variables ---
	b1 := 5
	b2 := 5
	code1 := ""
	code2 := ""

	// --- Read input file ---
	file, err := os.Open(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		for _, c := range line {
			switch c {
			case 'U':
				if b1 >= 4 {
					b1 -= 3
				}
				if b2 == 3 || b2 == 13 {
					b2 -= 2
				} else if (b2 >= 6 && b2 <= 8) || (b2 >= 10 && b2 <= 12) {
					b2 -= 4
				}
			case 'D':
				if b1 <= 6 {
					b1 += 3
				}
				if b2 == 1 || b2 == 11 {
					b2 += 2
				} else if (b2 >= 2 && b2 <= 4) || (b2 >= 6 && b2 <= 8) {
					b2 += 4
				}
			case 'L':
				if b1 != 1 && b1 != 4 && b1 != 7 {
					b1 -= 1
				}
				if (b2 >= 3 && b2 <= 4) || (b2 >= 6 && b2 <= 9) || (b2 >= 11 && b2 <= 12) {
					b2 -= 1
				}
			case 'R':
				if b1 != 3 && b1 != 6 && b1 != 9 {
					b1 += 1
				}
				if (b2 >= 2 && b2 <= 3) || (b2 >= 5 && b2 <= 8) || (b2 >= 10 && b2 <= 11) {
					b2 += 1
				}
			default:
				log.Panic("Unsupported rune:", string(c))
			}
		}
		code1 += strconv.Itoa(b1)
		if b2 < 10 {
			code2 += strconv.Itoa(b2)
		} else {
			code2 += string(65 + b2 - 10)
		}
	}

	// --- Results ---
	fmt.Println("1. Code:", code1)
	fmt.Println("2. Code:", code2)
}
