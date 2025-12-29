package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const PASSWORD = "abcdefgh"
const SCRAMBLED_PASSWORD = "fbgdceah"

// Swap positions
func swapPosition(s string, x, y int) string {
	r := []rune(s)
	r[x], r[y] = r[y], r[x]
	return string(r)
}

// Swap letters
func swapLetter(s string, a, b rune) string {
	r := []rune(s)
	for i := 0; i < len(r); i++ {
		if r[i] == a {
			r[i] = b
		} else if r[i] == b {
			r[i] = a
		}
	}
	return string(r)
}

// Reverse string
func reverse(s string, x, y int) string {
	a := []rune(s)
	b := make([]rune, len(a))
	for i := 0; i < x; i++ {
		b[i] = a[i]
	}
	for i := x; i <= y; i++ {
		b[i] = a[y+x-i]
	}
	for i := y + 1; i < len(a); i++ {
		b[i] = a[i]
	}
	return string(b)
}

// Rotate right
func rotateRight(s string, n int) string {
	r := []rune(s)
	i := n % len(r)
	i = len(r) - i
	return string(slices.Concat(r[i:], r[:i]))
}

// Rotate left
func rotateLeft(s string, n int) string {
	i := n % len(s)
	return rotateRight(s, len(s)-i)
}

// Rotate based on character
func rotate(s string, a rune) string {
	for i := 0; i < len(s); i++ {
		if rune(s[i]) == a {
			n := i + 1
			if i >= 4 {
				n++
			}
			return rotateRight(s, n)
		}
	}
	return s
}

// De-rotate based on characer
func derotate(s string, a rune) string {
	for i := 1; i < len(s); i++ {
		test := rotateLeft(s, i)
		//fmt.Println("-", test, rotate(test, a), rotate(test, a) == s)
		if rotate(test, a) == s {
			return test
		}
	}
	return s
}

// Move position X to Y
func move(s string, x, y int) string {
	r := []rune(s)
	temp := r[x]
	if x < y {
		for i := x; i < y; i++ {
			r[i] = r[i+1]
		}
	} else {
		for i := x; i > y; i-- {
			r[i] = r[i-1]
		}
	}
	r[y] = temp
	return string(r)
}

// Execute the code
func execute(code []string, input string) string {
	parseInt := func(s string) int {
		n, err := strconv.Atoi(s)
		if err != nil {
			log.Panic(err)
		}
		return n
	}

	parseRune := func(s string) rune {
		return []rune(s)[0]
	}

	re1 := regexp.MustCompile(`swap position (\d+) with position (\d+)`)
	re2 := regexp.MustCompile(`swap letter (\w) with letter (\w)`)
	re3 := regexp.MustCompile(`rotate (left|right) (\d+) step`)
	re4 := regexp.MustCompile(`rotate based on position of letter (\w)`)
	re5 := regexp.MustCompile(`reverse positions (\d+) through (\d+)`)
	re6 := regexp.MustCompile(`move position (\d+) to position (\d+)`)
	pwd := input
	for _, line := range code {
		if strings.HasPrefix(line, "swap position") {
			m := re1.FindStringSubmatch(line)
			pwd = swapPosition(pwd, parseInt(m[1]), parseInt(m[2]))
		} else if strings.HasPrefix(line, "swap letter") {
			m := re2.FindStringSubmatch(line)
			pwd = swapLetter(pwd, parseRune(m[1]), parseRune(m[2]))
		} else if strings.HasPrefix(line, "rotate based") {
			m := re4.FindStringSubmatch(line)
			pwd = rotate(pwd, parseRune(m[1]))
		} else if strings.HasPrefix(line, "derotate based") {
			m := re4.FindStringSubmatch(line)
			pwd = derotate(pwd, parseRune(m[1]))
		} else if strings.HasPrefix(line, "rotate") {
			m := re3.FindStringSubmatch(line)
			if m[1] == "left" {
				pwd = rotateLeft(pwd, parseInt(m[2]))
			} else if m[1] == "right" {
				pwd = rotateRight(pwd, parseInt(m[2]))
			} else {
				log.Panic("Invalid option:", m[1])
			}
		} else if strings.HasPrefix(line, "reverse positions") {
			m := re5.FindStringSubmatch(line)
			pwd = reverse(pwd, parseInt(m[1]), parseInt(m[2]))
		} else if strings.HasPrefix(line, "move position") {
			m := re6.FindStringSubmatch(line)
			pwd = move(pwd, parseInt(m[1]), parseInt(m[2]))
		} else if strings.HasPrefix(line, "demove position") {
			m := re6.FindStringSubmatch(line)
			pwd = move(pwd, parseInt(m[2]), parseInt(m[1]))
		} else {
			log.Panic("Unsupported command:", line)
		}
	}
	return pwd
}

func main() {
	// --- Variables ---
	code := []string{}

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
		code = append(code, line)
	}

	// --- Puzzle 1 ---
	pwd := execute(code, PASSWORD)
	fmt.Println("1. Scrambled pasword:", pwd)

	// --- Puzzle 2 ---
	slices.Reverse(code)
	for i := 0; i < len(code); i++ {
		if strings.HasPrefix(code[i], "rotate based") {
			code[i] = "de" + code[i]
		} else if strings.HasPrefix(code[i], "rotate left") {
			code[i] = strings.ReplaceAll(code[i], "left", "right")
		} else if strings.HasPrefix(code[i], "rotate right") {
			code[i] = strings.ReplaceAll(code[i], "right", "left")
		} else if strings.HasPrefix(code[i], "move position") {
			code[i] = "de" + code[i]
		}
	}
	pwd = execute(code, SCRAMBLED_PASSWORD)
	fmt.Println("2. Un-scrambled pasword:", pwd)
}
