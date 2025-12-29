package main

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"log"
	"os"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

// --- Struct ---
type State struct {
	passcode string
	position int
}

// Calculates the MD5 hash
func hashMD5(s string) string {
	data := []byte(s)
	bytes := md5.Sum(data)
	return hex.EncodeToString(bytes[:])
}

// Get the open options
func openOptions(passcode string) (bool, bool, bool, bool) {
	a := [4]bool{}
	hash := hashMD5(passcode)
	for i := 0; i < 4; i++ {
		a[i] = hash[i] >= 'b' && hash[i] <= 'f'
	}
	return a[0], a[1], a[2], a[3]
}

// Find the shortest (for puzzle = 1) or longest (for puzzle = 2) path
func findPath(passcode string, puzzle int) string {
	state := State{passcode: passcode, position: 1}
	queue := []State{state}
	last := ""
	for {
		if len(queue) == 0 {
			if puzzle == 1 {
				log.Panic("Path not found!")
			} else {
				return last
			}
		}

		state, queue = queue[0], queue[1:]
		if state.position == 16 {
			last = state.passcode[len(passcode):]
			if puzzle == 1 {
				return last
			}
			continue
		}

		u, d, l, r := openOptions(state.passcode)
		if u && state.position >= 5 {
			queue = append(queue, State{passcode: state.passcode + "U", position: state.position - 4})
		}
		if d && (state.position <= 12 || state.position == 16) {
			queue = append(queue, State{passcode: state.passcode + "D", position: state.position + 4})
		}
		if l && state.position != 1 && state.position != 5 && state.position != 9 && state.position != 13 {
			queue = append(queue, State{passcode: state.passcode + "L", position: state.position - 1})
		}
		if r && state.position != 4 && state.position != 8 && state.position != 12 {
			queue = append(queue, State{passcode: state.passcode + "R", position: state.position + 1})
		}
	}
}

func main() {
	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	seed := strings.TrimSpace(string(bytes))

	// --- Puzzle 1 ---
	passcode := findPath(seed, 1)
	fmt.Println("1. Shortest path:", passcode)

	// --- Puzzle 2 ---
	passcode = findPath(seed, 2)
	fmt.Println("2. Length of longest path:", len(passcode))
}
