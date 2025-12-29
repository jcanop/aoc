package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

func sanitize(s string) string {
	a := []rune(s)
	for i := 0; i < len(a); i++ {
		if a[i] == 'i' || a[i] == 'o' || a[i] == 'l' {
			a[i]++
			for j := 0; j < len(a); j++ {
				a[j] = 'a'
			}
			break
		}
	}
	return string(a)
}

func next(s string) string {
	a := []rune(s)
	for i := len(a) - 1; i >= 0; i-- {
		a[i]++
		if a[i] == 'i' || a[i] == 'o' || a[i] == 'l' {
			a[i]++
		}
		if a[i] > 'z' {
			a[i] = 'a'
		} else {
			break
		}
	}
	return string(a)
}

func rule1(a string) bool {
	for i := 0; i < len(a)-2; i++ {
		if a[i+1] == a[i]+1 && a[i+2] == a[i]+2 {
			return true
		}
	}
	return false
}

func rule2(a string) bool {
	c := byte(' ')
	n := 0
	for i := 0; i < len(a)-1; i++ {
		if a[i] == a[i+1] && c != a[i] {
			n++
			c = a[i]
		}
		if n == 2 {
			return true
		}
	}
	return false
}

// --- Constants ---
const FILENAME = "../input/input.txt"

func main() {
	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	pass := strings.TrimSpace(string(bytes))

	// --- Search next password ---
	count := 1
	pass = sanitize(pass)
	for {
		pass = next(pass)
		if rule1(pass) && rule2(pass) {
			fmt.Printf("%d. Next password: %s\n", count, pass)
			count++
			if count > 2 {
				break
			}
		}
	}
}
