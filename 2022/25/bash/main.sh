#!/bin/bash

# --- Constants ---
INPUT_FILE="../input/input.txt"

# --- SNAFU functions ---
to_snafu() {
	local VALUE=$1
	local BUFFER=""
	while [[ $VALUE -gt 0 ]]; do
		local OVERFLOW=0
		MOD=$(($VALUE % 5))
		if [[ $MOD -eq 0 ]]; then
			BUFFER="0$BUFFER"
		elif [[ $MOD -eq 1 ]]; then
			BUFFER="1$BUFFER"
		elif [[ $MOD -eq 2 ]]; then
			BUFFER="2$BUFFER"
		elif [[ $MOD -eq 3 ]]; then
			BUFFER="=$BUFFER"
			OVERFLOW=1
		elif [[ $MOD -eq 4 ]]; then
			BUFFER="-$BUFFER"
			OVERFLOW=1
		else
			echo "Unreachable!"
			exit 1
		fi
		VALUE=$(($VALUE / 5))
		if [[ $OVERFLOW -eq 1 ]]; then
			VALUE=$(($VALUE + 1))
		fi
	done
	echo $BUFFER
}

from_snafu() {
	local VALUE=$1
	local LEN=${#VALUE}
	local RESULT=0
	local i=0
	for c in $(echo $VALUE | grep -o .); do
		local p=$(( 5 ** ($LEN - $i - 1)))
		if [ $c == "0" ]; then
			: # NOOP
		elif [ $c == "1" ]; then
			RESULT=$(($RESULT + 1 * $p))
		elif [ $c == "2" ]; then
			RESULT=$(($RESULT + 2 * $p))
		elif [ $c == "=" ]; then
			RESULT=$(($RESULT - 2 * $p))
		elif [ $c == "-" ]; then
			RESULT=$(($RESULT - 1 * $p))
		else
			echo "Unreachable!"
			exit 1
		fi
		i=$(($i + 1))
	done
	echo $RESULT
}

# --- Read and parse file ---
TOTAL=0
while read line; do
	DEC=$(from_snafu $line)
	TOTAL=$(($TOTAL + $DEC))
done < $INPUT_FILE
SNAFU=$(to_snafu $TOTAL)
printf "SNAFU number to supply to Bob's console: %s\n" $SNAFU
