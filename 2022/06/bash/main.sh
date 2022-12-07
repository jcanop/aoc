#!/bin/bash

# --- Constants ---
INPUT_FILE="../input/input.txt"
C_PACKET=4
C_MESSAGE=14

# Indicates if a string has unique characters.
unique() {
	A=$(echo $1 | grep -o . |  wc -l)
	B=$(echo $1 | grep -o . |  sort | uniq | wc -l)
	if [ $A -eq $B ]; then
		return 0
	fi;
	return 1
}

# Search the stream of data for a marker.
find() {
	DATA=$1
	C=$2
	LEN=${#DATA}
	END=$(($LEN - $C))
	for i in $(seq 0 $END); do
		unique ${DATA:$i:$C} $C
		if [ $? -eq 0 ]; then
			RESULT=$((i + $C))
			return 0
		fi
	done
	RESULT=-1
}

# Search for the start of a packet.
find_packet() {
	find $1 $C_PACKET
}

# Search for the sstart of a message.
find_message() {
	find $1 $C_MESSAGE
}

# --- Read the input file ---
DATA=$(cat $INPUT_FILE)

# --- Puzzle 1 ---
find_packet $DATA
printf "Part 1. Start-of-packet marker: %'d\n" $RESULT

# --- Puzzle 2 ---
find_message $DATA
printf "Part 2. Start-of-message marker: %'d\n" $RESULT

