package main

import (
	"bufio"
	"fmt"
	"log"
	"math/rand"
	"os"
	"regexp"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const REGEX = `^([a-zA-Z]+) => ([a-zA-Z]+)$`

// --- Types ---
type MoleculeMap map[string][]string

func (m MoleculeMap) generateDistinctMolecules(molecule string) int {
	var keys []string
	for k := range m {
		keys = append(keys, regexp.QuoteMeta(k))
	}
	pattern := "(" + strings.Join(keys, "|") + ")"
	re := regexp.MustCompile(pattern)

	unique := make(map[string]struct{})
	pos := re.FindAllStringSubmatchIndex(molecule, -1)

	for _, match := range pos {
		i := match[2]
		j := match[3]
		from := molecule[i:j]

		for _, to := range m[from] {
			newMol := molecule[:i] + to + molecule[j:]
			unique[newMol] = struct{}{}
		}
	}

	return len(unique)
}

// --- Functions ---
func shuffle(slice []string) {
	n := len(slice)
	for i := n - 1; i > 0; i-- {
		j := rand.Intn(i + 1)
		slice[i], slice[j] = slice[j], slice[i]
	}
}

func findMinSteps(molecule string, m MoleculeMap, pairs []string) int {
	target := molecule
	steps := 0

	for target != "e" {
		change := false

		for _, pairStr := range pairs {
			parts := strings.Split(pairStr, "-")
			if len(parts) != 2 {
				continue
			}
			from, to := parts[0], parts[1]

			if strings.Contains(target, to) {
				target = strings.Replace(target, to, from, 1)
				change = true
				break
			}
		}

		if !change {
			shuffle(pairs)
			target = molecule
			steps = 0
			continue
		}

		steps++
	}

	return steps
}

func main() {
	// --- Variables ---
	var moleculeMap MoleculeMap = make(MoleculeMap)
	var pairs []string
	var molecule string
	var first = true

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
		if line == "" {
			first = false
			continue
		}

		if first {
			re := regexp.MustCompile(REGEX)
			matches := re.FindStringSubmatch(line)
			if len(matches) == 3 {
				from, to := matches[1], matches[2]
				if _, exists := moleculeMap[from]; !exists {
					moleculeMap[from] = []string{}
				}
				moleculeMap[from] = append(moleculeMap[from], to)
				pairs = append(pairs, from+"-"+to)
			}
		} else {
			molecule = line
		}
	}

	// --- Puzzle 1 ---
	count := moleculeMap.generateDistinctMolecules(molecule)
	fmt.Println("1. Distinct molecules can be created:", count)

	// --- Puzzle 2 ---
	steps := findMinSteps(molecule, moleculeMap, pairs)
	fmt.Println("2. Fewest number of steps:", steps)
}
