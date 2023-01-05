#!/bin/bash

# --- Constants ---
INPUT_FILE="../input/input.txt"
REGEX="move ([0-9]+) from ([0-9]+) to ([0-9]+)"

LEN=$(head -n1 $INPUT_FILE | wc -c)
COUNT=$(($LEN / 4))
QUEUES=()

# --- Read and parse the input file ---
while read -r line; do
	for ((i=0; i<$COUNT; i++)); do
		p=$(($i * 4 + 1))
		c=${line:$p:1}
		if [ "$c" != " " ]; then
			QUEUES[$i]="${QUEUES[$i]}$c"
		fi
	done
done < <(sed -e '/^$/,$d' $INPUT_FILE | head -n -1)

# --- Puzzle 1 ---
queues=("${QUEUES[@]}")
while read -r line; do
	[[ $line =~ $REGEX ]]
	i="${BASH_REMATCH[1]}"
	from=$((${BASH_REMATCH[2]} - 1))
	to=$((${BASH_REMATCH[3]} - 1))
	for ((j=0; j<$i; j++)); do
		queue="${queues[$from]}"
		c="${queue:0:1}"
		queues[$from]="${queue:1}"
		queues[$to]="$c${queues[$to]}"
	done
done < <(sed '0,/^$/d' $INPUT_FILE)

echo -n "1. Crates on the top of each stack: "
for list in "${queues[@]}"; do
	echo -n ${list:0:1}
done
echo ""


# --- Puzzle 2 ---
queues=("${QUEUES[@]}")
while read -r line; do
	[[ $line =~ $REGEX ]]
	i="${BASH_REMATCH[1]}"
	from=$((${BASH_REMATCH[2]} - 1))
	to=$((${BASH_REMATCH[3]} - 1))
	queue="${queues[$from]}"
	c="${queue:0:$i}"
	queues[$from]="${queue:$i}"
	queues[$to]="$c${queues[$to]}"
done < <(sed '0,/^$/d' $INPUT_FILE)

echo -n "2. Crates on the top of each stack: "
for list in "${queues[@]}"; do
	echo -n ${list:0:1}
done
echo ""

