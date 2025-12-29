package main

import (
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// --- Constants ---
const FILENAME = "../input/input.txt"
const REGEX = `Hit Points: (\d+).*Damage: (\d+).*Armor: (\d+)`
const PLAYER_HP = 100

var WEAPONS []Item = []Item{{8, 4, 0}, Item{10, 5, 0}, Item{25, 6, 0}, Item{40, 7, 0}, Item{74, 8, 0}}
var ARMORS []Item = []Item{{13, 0, 1}, Item{31, 0, 2}, Item{53, 0, 3}, Item{75, 0, 4}, Item{102, 0, 5}}
var RINGS []Item = []Item{{25, 1, 0}, Item{50, 2, 0}, Item{100, 3, 0}, Item{20, 0, 1}, Item{40, 0, 2}, Item{80, 0, 3}}

// --- Structs ---
type Item struct {
	cost   int
	damage int
	armor  int
}

type Player struct {
	hp     int
	damage int
	armor  int
	cost   int
}

// Creates a new Player struct
func newPlayer(hp int, weapon, armor, ring1, ring2 *Item) Player {
	cost := weapon.cost
	damage := weapon.damage
	shield := weapon.armor
	if armor != nil {
		cost += armor.cost
		damage += armor.damage
		shield += armor.armor
	}
	if ring1 != nil {
		cost += ring1.cost
		damage += ring1.damage
		shield += ring1.armor
	}
	if ring2 != nil {
		cost += ring2.cost
		damage += ring2.damage
		shield += ring2.armor
	}
	return Player{hp: hp, damage: damage, armor: shield, cost: cost}
}

// Play the game
func play(player, boss *Player) bool {
	ph := player.hp
	bh := boss.hp
	for ph > 0 {
		hit := player.damage - boss.armor
		if hit <= 0 {
			bh -= 1
		} else {
			bh -= hit
		}
		if bh <= 0 {
			return true
		}

		hit = boss.damage - player.armor
		if hit <= 0 {
			ph -= 1
		} else {
			ph -= hit
		}
	}
	return false
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

	// --- Variables ---
	re := regexp.MustCompile(REGEX)
	mn := math.MaxInt
	mx := math.MinInt

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))
	text = strings.ReplaceAll(text, "\n", " ")
	m := re.FindStringSubmatch(text)
	hp := parseInt(m[1])
	damage := parseInt(m[2])
	armor := parseInt(m[3])
	cost := 0
	boss := Player{hp, damage, armor, cost}

	// --- Combinations ---
	for w := 0; w < len(WEAPONS); w++ {
		for a := 0; a <= len(ARMORS); a++ {
			for r1 := 0; r1 <= len(RINGS); r1++ {
				for r2 := 0; r2 <= len(RINGS); r2++ {
					if r1 > 0 && r1 == r2 {
						continue
					}
					weapon := &WEAPONS[w]
					var armor *Item
					var ring1 *Item
					var ring2 *Item
					if a > 0 {
						armor = &ARMORS[a-1]
					}
					if r1 > 0 {
						ring1 = &RINGS[r1-1]
					}
					if r2 > 0 {
						ring2 = &RINGS[r2-1]
					}
					player := newPlayer(PLAYER_HP, weapon, armor, ring1, ring2)
					if play(&player, &boss) {
						mn = min(mn, player.cost)
					} else {
						mx = max(mx, player.cost)
					}
				}
			}
		}
	}

	// --- Results ---
	fmt.Println("1. Least amount of gold and win:", mn)
	fmt.Println("2. Most amount of gold and loose:", mx)
}
