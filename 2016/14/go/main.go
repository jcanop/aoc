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

// Calculates the MD5 hash
func hashMD5(seed string, i int) string {
	data := []byte(seed + strconv.Itoa(i))
	bytes := md5.Sum(data)
	return hex.EncodeToString(bytes[:])
}

// Calculates the streched MD5 hash
func strechedHashMD5(seed string, i int) string {
	hash := hashMD5(seed, i)
	for j := 0; j < 2016; j++ {
		data := []byte(hash)
		bytes := md5.Sum(data)
		hash = hex.EncodeToString(bytes[:])
	}
	return hash
}

// Finds if a hash has 3 consecutive characters
func findKey(hash string) (string, bool) {
	for i := 0; i < len(hash)-2; i++ {
		if hash[i] != hash[i+1] || hash[i] != hash[i+2] {
			continue
		}
		return strings.Repeat(string(hash[i]), 5), true
	}
	return "", false
}

// Searches for the one time keys
func findOnetimes(seed string, n, p int) []int {
	cache := make(map[int]string)
	onetime := []int{}

	// Searches for the one time keys
	i := 0
	for {
		hash, exists := cache[i]
		if !exists {
			switch p {
			case 1:
				hash = hashMD5(seed, i)
			case 2:
				hash = strechedHashMD5(seed, i)
			default:
				log.Panic("Unsupported puzzle:", p)
			}
			cache[i] = hash
		} else {
			delete(cache, i)
		}

		key, found := findKey(hash)
		if found {
			found = false
			for j := 1; j < 1000; j++ {
				h, exists := cache[i+j]
				if !exists {
					switch p {
					case 1:
						h = hashMD5(seed, i+j)
					case 2:
						h = strechedHashMD5(seed, i+j)
					default:
						log.Panic("Unsupported puzzle:", p)
					}
					cache[i+j] = h
				}
				if strings.Contains(h, key) {
					found = true
					break
				}
			}
			if found {
				onetime = append(onetime, i)
			}
		}

		if len(onetime) == n {
			return onetime
		}
		i++
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
	onetime := findOnetimes(seed, 64, 1)
	fmt.Println("1. Index of the 64th key:", onetime[63])

	// --- Puzzle 2 ---
	onetime = findOnetimes(seed, 64, 2)
	fmt.Println("2. Index of the 64th key:", onetime[63])
}
