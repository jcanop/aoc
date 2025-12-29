package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

// --- Global Variables ---
var outerRE = regexp.MustCompile(`\[[^\[\]]*\]`)
var insideRE = regexp.MustCompile(`\[([^\[\]]*)\]`)

// Checks if a string has a ABBA sequence
func hasABBA(s string) bool {
	for i := 0; i < len(s)-3; i++ {
		if s[i] == s[i+3] && s[i+1] == s[i+2] && s[i] != s[i+1] {
			return true
		}
	}
	return false
}

// Validate if an ip has a ABBA sequence
func validateABBA(ip string) bool {
	for _, s := range outerRE.Split(ip, -1) {
		if hasABBA(s) {
			return true
		}
	}
	return false
}

// Validate if an ip has not ABBA sequence in the hypernet sections
func validateNotABBAInHypernet(ip string) bool {
	for _, m := range insideRE.FindAllStringSubmatch(ip, -1) {
		for i := 1; i < len(m); i++ {
			if hasABBA(m[i]) {
				return false
			}
		}
	}
	return true
}

// Gets all the ABA sequences from an ip
func getABAs(ip string) []string {
	list := []string{}
	for _, s := range outerRE.Split(ip, -1) {
		for i := 0; i < len(s)-2; i++ {
			if s[i] == s[i+2] && s[i] != s[i+1] {
				list = append(list, string(s[i:i+3]))
			}
		}
	}
	return list
}

// Validates an ip that has at least one BAB sequence in the nypernet sections
func validateBAB(ip string, abas []string) bool {
	for _, aba := range abas {
		bab := string(aba[1:]) + string(aba[1])
		for _, m := range insideRE.FindAllStringSubmatch(ip, -1) {
			for i := 1; i < len(m); i++ {
				if strings.Contains(m[i], bab) {
					return true
				}
			}
		}
	}
	return false
}

func main() {
	// --- Variables ---
	count1 := 0
	count2 := 0

	// --- Read input file ---
	file, err := os.Open(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// --- Parse each line ---
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		ip := strings.TrimSpace(scanner.Text())

		// --- Puzzle 1 ----
		if validateABBA(ip) && validateNotABBAInHypernet(ip) {
			count1++
		}

		// --- Puzzle 2 ---
		abas := getABAs(ip)
		if validateBAB(ip, abas) {
			count2++
		}
	}

	// --- Results ---
	fmt.Println("1. Support TLS:", count1)
	fmt.Println("2. Support SSL:", count2)
}
