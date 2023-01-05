#!/bin/bash

# --- Constants ---
INPUT_FILE="../input/input.txt"
LIMIT=100000
DISK_SIZE=70000000
UPDATE_SIZE=30000000

# --- Global Variables ---
declare -A DATA
DIRS=()
LEVEL=0

# --- Read and parse the input file ---
while read -r line; do
	if [ "$line" == "$ cd .." ]; then
		LEVEL=$((LEVEL - 1))
		unset -v "DIRS[$LEVEL]"
	elif [ "${line:0:4}" == "$ cd" ]; then
		DIRS[$LEVEL]="${line:5}"
		printf -v DIR '%s/' "${DIRS[@]}"
		DIR="${DIR:1}"
		DATA[$DIR]=0
		LEVEL=$((LEVEL + 1))
	elif [[ $line =~ ^([0-9]+).+$ ]]; then
		for ((i=1; i<=$LEVEL; i++)); do
			printf -v DIR '%s/' "${DIRS[@]:0:$i}"
			DIR="${DIR:1}"
			DATA[$DIR]=$((${DATA[$DIR]} + ${BASH_REMATCH[1]}))
		done
	fi
done < $INPUT_FILE

# --- Puzzle 1 ---
TOTAL=0
for key in "${!DATA[@]}"; do
	SIZE=${DATA[$key]}
	if [[ $SIZE -le $LIMIT ]]; then
		TOTAL=$(($TOTAL + $SIZE))
	fi
done
printf "1. The sum of the total sizes of the directories under %'d is %'d\n" $LIMIT $TOTAL

# --- Puzzle 2 ---
NEED=$(( $UPDATE_SIZE - $DISK_SIZE + ${DATA['/']} ))
LIST=()
for key in "${!DATA[@]}"; do
	SIZE=${DATA[$key]}
	if [[ $SIZE -ge $NEED ]]; then
		LIST+=( $SIZE )
	fi
done
SORTED=($(tr ' ' '\n' <<<"${LIST[@]}" | sort -n))
printf "2. The total size of the smalles directory needed is %'d\n" ${SORTED[0]}
