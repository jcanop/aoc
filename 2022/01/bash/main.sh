#!/bin/bash

INPUT_FILE="../input/input.txt"

# --- Read and parse the input file ---
list=()
total=0
while IFS= read -r line; do
	if [ -z $line ]; then
		list+=($total)
		total=0
	else
		total=$(($total + $line))
	fi
done <<< $(cat $INPUT_FILE)
list+=($total)
list=( $(printf "%s\n" "${list[@]}" | sort -nr) )


# --- Find the Elf carrying the most Calories ---
max=${list[0]}
printf "The Elf carrying the most Calories, is carrying %'d Calories.\n" $max

# --- Find the top three Elves carrying the most Calories ---
total=$((${list[0]} + ${list[1]} + ${list[2]}))
printf "The top 3 Elves carrying the most Calores, are carrying %'d Calories.\n" $total
