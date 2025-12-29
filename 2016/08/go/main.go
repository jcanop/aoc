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
const WIDTH = 50
const HEIGHT = 6

func createScreen() [WIDTH][HEIGHT]bool {
	return [WIDTH][HEIGHT]bool{}
}

// Prints the screen
func printScreen(screen *[WIDTH][HEIGHT]bool) {
	for y := 0; y < HEIGHT; y++ {
		for x := 0; x < WIDTH; x++ {
			v := screen[x][y]
			if v {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
}

// Draw a rect on the top left of the screen
func rect(screen *[WIDTH][HEIGHT]bool, w, h int) {
	for x := 0; x < w; x++ {
		for y := 0; y < h; y++ {
			screen[x][y] = true
		}
	}
}

// Rotate the screen
func rotate(screen *[WIDTH][HEIGHT]bool, t string, i, n int) {
	if t == "row" {
		temp := [WIDTH]bool{}
		for j := 0; j < WIDTH; j++ {
			temp[(j+n)%WIDTH] = screen[j][i]
		}
		for j := 0; j < WIDTH; j++ {
			screen[j][i] = temp[j]
		}
	} else if t == "column" {
		temp := [HEIGHT]bool{}
		for j := 0; j < HEIGHT; j++ {
			temp[(j+n)%HEIGHT] = screen[i][j]
		}
		for j := 0; j < HEIGHT; j++ {
			screen[i][j] = temp[j]
		}
	} else {
		log.Panic("Not supported type:", t)
	}
}

// Count the lit pixels on the screen
func count(screen *[WIDTH][HEIGHT]bool) int {
	result := 0
	for x := 0; x < WIDTH; x++ {
		for y := 0; y < HEIGHT; y++ {
			if screen[x][y] {
				result++
			}
		}
	}
	return result
}

func main() {
	// --- Variables ---
	re1 := regexp.MustCompile(`rect (\d+)x(\d+)`)
	re2 := regexp.MustCompile(`rotate (row|column) [xy]=(\d+) by (\d+)`)
	screen := createScreen()

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
		if strings.HasPrefix(line, "rect") {
			matches := re1.FindStringSubmatch(line)
			if matches == nil {
				log.Fatal("No matches found!")
			}
			w, err := strconv.Atoi(matches[1])
			if err != nil {
				log.Panic(err)
			}
			h, err := strconv.Atoi(matches[2])
			if err != nil {
				log.Panic(err)
			}
			rect(&screen, w, h)
		} else if strings.HasPrefix(line, "rotate") {
			matches := re2.FindStringSubmatch(line)
			if matches == nil {
				log.Fatal("No matches found!")
			}
			t := matches[1]
			i, err := strconv.Atoi(matches[2])
			if err != nil {
				log.Panic(err)
			}
			n, err := strconv.Atoi(matches[3])
			if err != nil {
				log.Panic(err)
			}
			rotate(&screen, t, i, n)
		}
	}

	// --- Puzzle 1 ---
	fmt.Println("1. Lit pixels:", count(&screen))

	// --- Puzzle 2 ---
	fmt.Println("2. Code is the screen:")
	printScreen(&screen)
}
