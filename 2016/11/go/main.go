package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const FLOORS = 4
const ELV uint64 = 0b1000000000000000
const COG uint64 = 0b0010000000000000
const POG uint64 = 0b0001000000000000
const PMG uint64 = 0b0000100000000000
const TMG uint64 = 0b0000010000000000
const RUG uint64 = 0b0000001000000000
const ELG uint64 = 0b0000000100000000
const DTG uint64 = 0b0000000010000000
const COC uint64 = 0b0000000001000000
const POC uint64 = 0b0000000000100000
const PMC uint64 = 0b0000000000010000
const TMC uint64 = 0b0000000000001000
const RUC uint64 = 0b0000000000000100
const ELC uint64 = 0b0000000000000010
const DTC uint64 = 0b0000000000000001

var ELS1 = []uint64{COG, POG, PMG, TMG, RUG, COC, POC, PMC, TMC, RUC}
var ELS2 = []uint64{COG, POG, PMG, TMG, RUG, ELG, DTG, COC, POC, PMC, TMC, RUC, ELC, DTC}

// --- Structs ---
type State struct {
	floors uint64 // [F1][F2][F3][F4]
	steps  uint
}

// Finds where the elevator is
func (state *State) getElevatorFloor() int {
	s := state.floors
	for f := FLOORS - 1; f >= 0; f-- {
		if s&ELV == ELV {
			return f
		}
		s = s >> 16
	}
	return -1
}

// Checks if a element is in a floor
func (state *State) has(f int, e uint64) bool {
	return (state.floors>>((FLOORS-f-1)*16))&e == e
}

// Remove a element from the floor
func (state *State) remove(f int, e uint64) {
	state.floors ^= (e << ((FLOORS - f - 1) * 16))
}

// Set a element in the floor
func (state *State) set(f int, e uint64) {
	state.floors |= (e << ((FLOORS - f - 1) * 16))
}

// Checks if the state is valid
func (s *State) isValid(i int) bool {
	for f := 0; f < FLOORS; f++ {
		// --- Has the elevator ---
		if f == i && !s.has(f, ELV) {
			return false
		}

		// --- Check chips and generators ---
		if (s.has(f, COC) && !s.has(f, COG) && (s.has(f, POG) || s.has(f, PMG) || s.has(f, TMG) || s.has(f, RUG) || s.has(f, ELG) || s.has(f, DTG))) ||
			(s.has(f, POC) && !s.has(f, POG) && (s.has(f, COG) || s.has(f, PMG) || s.has(f, TMG) || s.has(f, RUG) || s.has(f, ELG) || s.has(f, DTG))) ||
			(s.has(f, PMC) && !s.has(f, PMG) && (s.has(f, COG) || s.has(f, POG) || s.has(f, TMG) || s.has(f, RUG) || s.has(f, ELG) || s.has(f, DTG))) ||
			(s.has(f, TMC) && !s.has(f, TMG) && (s.has(f, COG) || s.has(f, POG) || s.has(f, PMG) || s.has(f, RUG) || s.has(f, ELG) || s.has(f, DTG))) ||
			(s.has(f, RUC) && !s.has(f, RUG) && (s.has(f, COG) || s.has(f, POG) || s.has(f, PMG) || s.has(f, TMG) || s.has(f, ELG) || s.has(f, DTG))) ||
			(s.has(f, ELC) && !s.has(f, ELG) && (s.has(f, COG) || s.has(f, POG) || s.has(f, PMG) || s.has(f, TMG) || s.has(f, RUG) || s.has(f, DTG))) ||
			(s.has(f, DTC) && !s.has(f, DTG) && (s.has(f, COG) || s.has(f, POG) || s.has(f, PMG) || s.has(f, TMG) || s.has(f, RUG) || s.has(f, ELG))) {
			return false
		}
	}
	return true
}

// checks if it is the target state
func (s *State) isDone(elems []uint64) bool {
	// --- Check elevator ---
	if !s.has(FLOORS-1, ELV) {
		return false
	}

	// --- Check elements ---
	for _, e := range elems {
		if !s.has(FLOORS-1, e) {
			return false
		}
	}
	return true
}

// Prints the state to the console, used for debuggig
func printState(state State, elems []uint64) {
	if len(elems) == 10 {
		fmt.Println("   E COG POG PMG TMG RUG COC POC PMC TMC RUC")
	} else {
		fmt.Println("   E COG POG PMG TMG RUG ELG DTG COC POC PMC TMC RUC ELC DTC")
	}
	s := state.floors
	for f := FLOORS - 1; f >= 0; f-- {
		fmt.Printf("F%d ", f+1)
		if s&ELV == ELV {
			fmt.Print("# ")
		} else {
			fmt.Print(". ")
		}
		for i := 0; i < len(elems); i++ {
			if s&elems[i] == elems[i] {
				fmt.Print(" #  ")
			} else {
				fmt.Print(" .  ")
			}
		}
		fmt.Println()
		s = s >> 16
	}
	fmt.Println("Steps:", state.steps)
}

// Search for the best path
func run(state State, elems []uint64) uint {
	queue := []State{state}
	set := make(map[uint64]struct{})

	for {
		if len(queue) == 0 {
			log.Println("Could not find the answer!")
		}

		state, queue = queue[0], queue[1:]
		f := state.getElevatorFloor()
		for i := -1; i <= 1; i++ {
			if (f == 0 && i == -1) || i == 0 || (f == FLOORS-1 && i == 1) {
				continue
			}
			n := f + i // New floor
			s1 := state

			// --- Move Elevator ---
			s1.remove(f, ELV)
			s1.set(n, ELV)

			// --- Move each element ---
			for _, e := range elems {
				if !s1.has(f, e) {
					continue
				}
				s2 := s1
				s2.remove(f, e)
				s2.set(n, e)
				s2.steps++
				if s2.isValid(n) {
					// --- Checks if it is the final state ---
					if s2.isDone(elems) {
						return s2.steps
					}
					// --- Skip previous states ---
					_, exists := set[s2.floors]
					if !exists {
						queue = append(queue, s2)
						set[s2.floors] = struct{}{}
					}
				}
			}

			// --- Move two elements at once ---
			for j := 0; j < len(elems); j++ {
				for k := j + 1; k < len(elems); k++ {
					if !s1.has(f, elems[j]) || !s1.has(f, elems[k]) {
						continue
					}
					s2 := s1
					s2.remove(f, elems[j])
					s2.remove(f, elems[k])
					s2.set(n, elems[j])
					s2.set(n, elems[k])
					s2.steps++
					if s2.isValid(n) {
						// --- Checks if it is the final state ---
						if s2.isDone(elems) {
							return s2.steps
						}
						// --- Skip previous states ---
						_, exists := set[s2.floors]
						if !exists {
							queue = append(queue, s2)
							set[s2.floors] = struct{}{}
						}
					}
				}
			}
		}
	}
}

func main() {
	// --- Read input file ---
	file, err := os.Open(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// --- Parse each line ---
	state := State{}
	scanner := bufio.NewScanner(file)
	for f := 0; f < FLOORS; f++ {
		scanner.Scan()
		line := strings.TrimSpace(scanner.Text())

		state.floors = state.floors << 16
		if f == 0 {
			state.floors |= ELV
		}
		if strings.Contains(line, "cobalt generator") {
			state.floors |= COG
		}
		if strings.Contains(line, "polonium generator") {
			state.floors |= POG
		}
		if strings.Contains(line, "promethium generator") {
			state.floors |= PMG
		}
		if strings.Contains(line, "thulium generator") {
			state.floors |= TMG
		}
		if strings.Contains(line, "ruthenium generator") {
			state.floors |= RUG
		}
		if strings.Contains(line, "cobalt-compatible microchip") {
			state.floors |= COC
		}
		if strings.Contains(line, "polonium-compatible microchip") {
			state.floors |= POC
		}
		if strings.Contains(line, "promethium-compatible microchip") {
			state.floors |= PMC
		}
		if strings.Contains(line, "thulium-compatible microchip") {
			state.floors |= TMC
		}
		if strings.Contains(line, "ruthenium-compatible microchip") {
			state.floors |= RUC
		}
	}

	// --- Puzzle 1 ---
	steps := run(state, ELS1)
	fmt.Println("1. Minimum number of steps:", steps)

	// --- Puzzle 2 ---
	state.set(0, ELC)
	state.set(0, ELG)
	state.set(0, DTC)
	state.set(0, DTG)
	steps = run(state, ELS2)
	fmt.Println("2. Minimum number of steps:", steps)
}
