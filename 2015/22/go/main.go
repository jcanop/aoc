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
const REGEX = `Hit Points: (\d+).*Damage: (\d+)`
const PLAYER_HP = 50
const PLAYER_MANA = 500
const RESULT_LOSE = -1
const RESULT_WIN = 1
const RESULT_CONTINUE = 0

var MAGIC_MISSILE = Magic{cost: 53, duration: 0, effect: 4}
var MAGIC_DRAIN = Magic{cost: 73, duration: 0, effect: 2}
var MAGIC_SHIELD = Magic{cost: 113, duration: 6, effect: 7}
var MAGIC_POISON = Magic{cost: 173, duration: 6, effect: 3}
var MAGIC_RECHARGE = Magic{cost: 229, duration: 5, effect: 101}

// --- Structs ---
type Magic struct {
	cost     int
	duration int
	effect   int
}

type State struct {
	playerHP    int
	playerArmor int
	playerMana  int
	shield      int
	poison      int
	recharge    int
	bossHP      int
	usedMana    int
}

func (state *State) applyEffects() {
	if state.shield > 0 {
		state.shield--
		if state.shield > 0 {
			state.playerArmor = MAGIC_SHIELD.effect
		} else {
			state.playerArmor = 0
		}
	}
	if state.poison > 0 {
		state.poison--
		state.bossHP -= MAGIC_POISON.effect
	}
	if state.recharge > 0 {
		state.recharge--
		state.playerMana += MAGIC_RECHARGE.effect
	}
}

func (state *State) canCast(magic *Magic) bool {
	if state.playerMana <= magic.cost {
		return false
	}
	if *magic == MAGIC_SHIELD {
		return state.shield == 0
	}
	if *magic == MAGIC_POISON {
		return state.poison == 0
	}
	if *magic == MAGIC_RECHARGE {
		return state.recharge == 0
	}
	return true
}

func (state *State) cast(magic *Magic, bossDamage int) int {
	// --- Players turn ---
	state.playerMana -= magic.cost
	state.usedMana += magic.cost
	if *magic == MAGIC_MISSILE {
		state.bossHP -= magic.effect
	} else if *magic == MAGIC_DRAIN {
		state.bossHP -= magic.effect
		state.playerHP += magic.effect
	} else if *magic == MAGIC_SHIELD {
		state.shield = magic.duration
	} else if *magic == MAGIC_POISON {
		state.poison = magic.duration
	} else if *magic == MAGIC_RECHARGE {
		state.recharge = magic.duration
	}

	// --- Boss turn ---
	state.applyEffects()
	if state.bossHP <= 0 {
		return RESULT_WIN
	}
	hit := max(bossDamage-state.playerArmor, 1)
	state.playerHP -= hit
	if state.playerHP <= 0 {
		return RESULT_LOSE
	}
	return RESULT_CONTINUE
}

func play(bossHP, bossDamage int, hardmode bool) int {
	spells := []Magic{MAGIC_DRAIN, MAGIC_SHIELD, MAGIC_POISON, MAGIC_RECHARGE}
	queue := []State{}
	mn := math.MaxInt

	state := State{playerHP: PLAYER_HP, playerArmor: 0, playerMana: PLAYER_MANA, shield: 0, poison: 0, recharge: 0, bossHP: bossHP, usedMana: 0}
	queue = append(queue, state)
	for len(queue) > 0 {
		state, queue = queue[0], queue[1:]

		// --- Ignore worst paths ---
		if state.usedMana >= mn {
			continue
		}

		// --- Hard mode ---
		if hardmode {
			state.playerHP--
			if state.playerHP <= 0 {
				continue
			}
		}

		// --- Apply player turn effects ---
		state.applyEffects()
		if state.bossHP <= 0 {
			mn = min(mn, state.usedMana)
			continue
		}

		// --- Try every spell except Missile ---
		for _, spell := range spells {
			if state.canCast(&spell) {
				clone := state
				result := clone.cast(&spell, bossDamage)
				if result == RESULT_WIN {
					mn = min(mn, clone.usedMana)
				} else if result == RESULT_CONTINUE {
					queue = append(queue, clone)
				}
			}
		}

		// --- Try cast Missile ---
		if state.canCast(&MAGIC_MISSILE) {
			result := state.cast(&MAGIC_MISSILE, bossDamage)
			if result == RESULT_WIN {
				mn = min(mn, state.usedMana)
			} else if result == RESULT_CONTINUE {
				queue = append(queue, state)
			}
		}
	}

	return mn
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

	// --- Read input file ---
	bytes, err := os.ReadFile(FILENAME)
	if err != nil {
		log.Fatal(err)
	}
	text := strings.TrimSpace(string(bytes))
	text = strings.ReplaceAll(text, "\n", " ")
	m := re.FindStringSubmatch(text)
	bossHP := parseInt(m[1])
	bossDamage := parseInt(m[2])

	// --- Puzzle 1 ---
	mn := play(bossHP, bossDamage, false)
	fmt.Println("1. Least amount of mana:", mn)

	// --- Puzzle 2 ---
	mn = play(bossHP, bossDamage, true)
	fmt.Println("2. Least amount of mana:", mn)
}
