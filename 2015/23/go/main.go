package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

// --- Structs ---
type CPU struct {
	a int
	b int
}

func (cpu *CPU) set(reg rune, i int) {
	switch reg {
	case 'a':
		cpu.a = i
	case 'b':
		cpu.b = i
	}
}

func (cpu *CPU) get(reg rune) int {
	switch reg {
	case 'a':
		return cpu.a
	case 'b':
		return cpu.b
	}
	return 0
}

func (cpu *CPU) exec(code []string) {
	for i := 0; i < len(code); i++ {
		s := strings.Fields(code[i])
		s[1] = strings.ReplaceAll(s[1], ",", "")
		reg := rune(s[1][0])
		switch s[0] {
		case "hlf":
			cpu.set(reg, cpu.get(reg)/2)
		case "tpl":
			cpu.set(reg, cpu.get(reg)*3)
		case "inc":
			cpu.set(reg, cpu.get(reg)+1)
		case "jmp":
			i += parseInt(s[1]) - 1
		case "jie":
			if cpu.get(reg)%2 == 0 {
				i += parseInt(s[2]) - 1
			}
		case "jio":
			if cpu.get(reg) == 1 {
				i += parseInt(s[2]) - 1
			}
		default:
			log.Panic("Unsupported op:", s[0])
		}
	}
}

func parseInt(s string) int {
	n, err := strconv.Atoi(s)
	if err != nil {
		log.Panic(err)
	}
	return n
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
	cpu := CPU{}
	cpu.exec(code)
	fmt.Println("1. Register b:", cpu.b)

	// --- Puzze 2 ---
	cpu = CPU{a: 1, b: 0}
	cpu.exec(code)
	fmt.Println("2. Register b:", cpu.b)
}
