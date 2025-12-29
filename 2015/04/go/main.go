package main

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"

func main() {
	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	secret := strings.TrimSpace(string(bytes))

	found1 := false
	found2 := false
	count := 0

	for {
		hasher := md5.New()
		text := secret + strconv.Itoa(count)
		hasher.Write([]byte(text))
		hash := hex.EncodeToString(hasher.Sum(nil))

		// --- Puzzle 1 ---
		if !found1 && hash[:5] == "00000" {
			fmt.Println("1. First hash with five zeros:", count)
			found1 = true
		}

		// --- Puzzle 2 ---
		if !found2 && hash[:6] == "000000" {
			fmt.Println("2. First hash with six zeros:", count)
			found2 = true
		}

		if found1 && found2 {
			break
		}
		count++
	}
}
