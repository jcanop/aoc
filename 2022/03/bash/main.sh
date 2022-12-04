#!/bin/bash

INPUT_FILE="../input/input.txt"

# Find the duplicated character on the arrays.
find_duplicates() {
	for a in $(echo $1 | grep -o .); do
		for b in $(echo $2 | grep -o .); do
			if [ "$a" = "$b" ]; then
				if [ "$3" = "" ]; then
					RESULT=$a
					return 0
				fi
				for c in $(echo $3 | grep -o .); do
					if [ "$b" = "$c" ]; then
						RESULT=$a
						return 0
					fi
				done
			fi
		done
	done
}

# Get the priority value of a character.
get_priority() {
	ascii=$(LC_CTYPE=C printf '%d' "'$1")
	if [[ "$1" =~ ^[a-z]$ ]]; then
		RESULT=$(($ascii - 96))
	elif [[ "$1" =~ ^[A-Z]$ ]]; then
		RESULT=$(($ascii - 64 + 26))
	fi
}

# --- Parse the input file ---
T1=0
T2=0
LINES=()
IDX=0
while read line; do
	# --- Puzzle 1 ---
	len=$((${#line} /2 ))
	S1=${line:0:$len}
	S2=${line:$len:$len}
	find_duplicates $S1 $S2
	get_priority $RESULT
	T1=$(($T1 + $RESULT))

	# --- Puzzle 2 ---
	LINES[$IDX]=$line
	IDX=$(($IDX + 1))
	if [ $IDX -eq 3 ]; then
		find_duplicates ${LINES[0]} ${LINES[1]} ${LINES[2]}
		get_priority $RESULT
		T2=$(($T2 + $RESULT))
		IDX=0
	fi
done < $INPUT_FILE
printf "Part 1. Total sum of the priorities: %'d\n" $T1
printf "Part 2. Total sum of the item types: %'d\n" $T2
