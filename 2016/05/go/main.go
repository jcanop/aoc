package main

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"log"
	"os"
	"slices"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

func main() {
	// --- Variables ---
	index := 0
	password1 := ""
	password2 := []rune("________")

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	room := strings.TrimSpace(string(bytes))

	for {
		// --- Calcualte the MD5 hash ---
		data := []byte(room + strconv.Itoa(index))
		bytes := md5.Sum(data)
		hash := hex.EncodeToString(bytes[:])

		if hash[:5] == "00000" {
			// ---- Puzzle 1 ---
			if len(password1) < 8 {
				password1 += string(hash[5])
			}

			// --- Puzzle 2 ---
			if hash[5] >= '0' && hash[5] <= '7' {
				i := int(hash[5] - '0')
				if password2[i] == '_' {
					password2[i] = rune(hash[6])
				}
			}
		}

		// --- Are we done? ---
		if len(password1) == 8 && !slices.Contains(password2, '_') {
			break
		}
		index++
	}

	// --- Results ---
	fmt.Println("1. Password:", password1)
	fmt.Println("2. Password:", string(password2))
}
