package main

import (
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const REGEX_1 = `Begin in state (\w). Perform a diagnostic checksum after (\d+) steps.`
const REGEX_2 = `In state (\w): If the current value is 0: - Write the value (\d). - Move one slot to the (right|left). - Continue with state (\w). If the current value is 1: - Write the value (\d+). - Move one slot to the (right|left). - Continue with state (\w).`

// --- Structs ---
type State struct {
	id     string
	zvalue int
	zmove  int
	znext  string
	ovalue int
	omove  int
	onext  string
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
	parseDir := func(s string) int {
		switch s {
		case "left":
			return -1
		case "right":
			return 1
		default:
			log.Panic("Unsupported option:", s)
		}
		return 0
	}

	// --- Variables ---
	var state string
	var steps int
	states := make(map[string]State)
	cursor := 0

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))

	// --- Parse input ---
	re := regexp.MustCompile(`\s+`)
	re1 := regexp.MustCompile(REGEX_1)
	re2 := regexp.MustCompile(REGEX_2)
	i := strings.Index(text, "\n\n")
	header := strings.ReplaceAll(text[:i], "\n", " ")
	body := text[i+2:]
	m := re1.FindStringSubmatch(header)
	state = m[1]
	steps = parseInt(m[2])
	for _, fs := range strings.Split(body, "\n\n") {
		s := strings.ReplaceAll(fs, "\n", " ")
		s = re.ReplaceAllString(s, " ")
		m = re2.FindStringSubmatch(s)
		id := m[1]
		zvalue := parseInt(m[2])
		zmove := parseDir(m[3])
		znext := m[4]
		ovalue := parseInt(m[5])
		omove := parseDir(m[6])
		onext := m[7]
		states[id] = State{id, zvalue, zmove, znext, ovalue, omove, onext}
	}

	// --- Puzzle ---
	tape := make(map[int]int)
	for i := 0; i < steps; i++ {
		s := states[state]
		switch tape[cursor] {
		case 0:
			tape[cursor] = s.zvalue
			cursor += s.zmove
			state = s.znext
		case 1:
			tape[cursor] = s.ovalue
			cursor += s.omove
			state = s.onext
		default:
			log.Panic("Invalid value:", tape[cursor])
		}
	}
	sum := 0
	for _, v := range tape {
		sum += v
	}
	fmt.Println("Diagnostic checksum:", sum)
}
