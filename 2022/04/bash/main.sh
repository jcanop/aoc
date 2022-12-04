#!/bin/bash

INPUT_FILE="../input/input.txt"

# Splits the string in two by a separator.
split() {
	value="$1"
	sep="$2"
	prefix=${value%%$sep*}
	index=${#prefix}
	RESULT1=${value:0:$index}
	RESULT2=${value:$(($index + 1))}
}

# Checks if this range fully contains another range.
fully_contains() {
	MIN1=$1
	MAX1=$2
	MIN2=$3
	MAX2=$4
	if [ $MIN1 -le $MIN2 ] && [ $MAX2 -le $MAX1 ]; then
		RESULT=1
	else
		RESULT=0
	fi
}

# Checks if this range overlaps with another range.
overlap() {
	MIN1=$1
	MAX1=$2
	MIN2=$3
	MAX2=$4
	if  ([ $MIN2 -ge $MIN1 ] && [ $MIN2 -le $MAX1 ]) ||
		([ $MAX2 -ge $MIN1 ] && [ $MAX2 -le $MAX1 ]) ||
		([ $MIN1 -ge $MIN2 ] && [ $MIN1 -le $MAX2 ]) ||
		([ $MAX1 -ge $MIN2 ] && [ $MAX1 -le $MAX2 ]); then
		RESULT=1
	else
		RESULT=0
	fi
}

# --- Parse the input file ---
T1=0
T2=0
while read line; do
	split $line ","
	S1=$RESULT1
	S2=$RESULT2
	split $S1 "-"
	S1_MIN=$RESULT1
	S1_MAX=$RESULT2
	split $S2 "-"
	S2_MIN=$RESULT1
	S2_MAX=$RESULT2

	# --- Puzzle 1 ---
	fully_contains $S1_MIN $S1_MAX $S2_MIN $S2_MAX
	T1=$(($T1 + $RESULT))
	if [ $RESULT -eq 0 ]; then
		fully_contains $S2_MIN $S2_MAX $S1_MIN $S1_MAX
		T1=$(($T1 + $RESULT))
	fi

	# --- Puzzle 2 ---
	overlap $S1_MIN $S1_MAX $S2_MIN $S2_MAX
	T2=$(($T2 + $RESULT))
done < $INPUT_FILE
printf "Part 1. Total of ranges that fully contains another: %'d\n" $T1
printf "Part 2. Total of ranges that overlaps: %'d\n" $T2
