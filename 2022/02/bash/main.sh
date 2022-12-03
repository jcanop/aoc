#!/bin/bash

INPUT_FILE="../input/input.txt"

SHAPE_ROCK="SR"
SHAPE_PAPER="SP"
SHAPE_SCISSORS="SS"
RESULT_WIN="RW"
RESULT_DRAW="RD"
RESULT_LOSE="RL"

parse_shape() {
	if [ "$1" = "A" ] || [ "$1" = "X" ]; then
		RESULT=$SHAPE_ROCK
	elif [ "$1" = "B" ] || [ "$1" = "Y" ]; then
		RESULT=$SHAPE_PAPER
	elif [ "$1" = "C" ] || [ "$1" = "Z" ]; then
		RESULT=$SHAPE_SCISSORS
	fi
}

parse_result() {
	if [ "$1" = "X" ]; then
		RESULT=$RESULT_LOSE
	elif [ "$1" = "Y" ]; then
		RESULT=$RESULT_DRAW
	elif [ "$1" = "Z" ]; then
		RESULT=$RESULT_WIN
	fi
}

# This function returns the shape to win, lose or draw against this shape.
#	$1 - Shape
#	$2 - Deseried result
shape_to() {
	if [ "$2" = "$RESULT_WIN" ]; then
		if [ "$1" = "$SHAPE_ROCK" ]; then
			RESULT=$SHAPE_PAPER
		elif [ "$1" = "$SHAPE_PAPER" ]; then
			RESULT=$SHAPE_SCISSORS
		elif [ "$1" = "$SHAPE_SCISSORS" ]; then
			RESULT=$SHAPE_ROCK
		fi
	elif [ "$2" = "$RESULT_LOSE" ]; then
		if [ "$1" = "$SHAPE_ROCK" ]; then
			RESULT=$SHAPE_SCISSORS
		elif [ "$1" = "$SHAPE_PAPER" ]; then
			RESULT=$SHAPE_ROCK
		elif [ "$1" = "$SHAPE_SCISSORS" ]; then
			RESULT=$SHAPE_PAPER
		fi
	elif [ "$2" = "$RESULT_DRAW" ]; then
		RESULT=$1
	fi
}

# This function matches two shapes and returns a result.
#	$1 - Me
#	$2 - Opponent
do_match() {
	if [ "$1" = "$2" ]; then
		RESULT=$RESULT_DRAW
	elif ([ "$1" = "$SHAPE_ROCK" ] && [ "$2" = "$SHAPE_SCISSORS" ]) ||
		([ "$1" = "$SHAPE_PAPER" ] && [ "$2" = "$SHAPE_ROCK" ]) ||
		([ "$1" = "$SHAPE_SCISSORS" ] && [ "$2" = "$SHAPE_PAPER" ]); then
		RESULT=$RESULT_WIN
	else
		RESULT=$RESULT_LOSE
	fi
}

# This function plays the game against this shape.
#	$1 - Me
#	$2 - Opponent
play() {
	do_match $1 $2
	RES=$RESULT

	if [ "$RES" = "$RESULT_DRAW" ]; then
		RESULT=3
	elif [ "$RES" = "$RESULT_WIN" ]; then
		RESULT=6
	fi

	if [ "$1" = "$SHAPE_ROCK" ]; then
		RESULT=$(($RESULT + 1))
	elif [ "$1" = "$SHAPE_PAPER" ]; then
		RESULT=$(($RESULT + 2))
	elif [ "$1" = "$SHAPE_SCISSORS" ]; then
		RESULT=$(($RESULT + 3))
	fi
}

# --- Puzzle: Part 1 ---
while read line; do
	parse_shape ${line:0:1}
	OP=$RESULT
	parse_shape ${line:2:1}
	ME=$RESULT
	play $ME $OP
	TOTAL=$(( $TOTAL + $RESULT))
done < $INPUT_FILE
printf "Part 1. Total score: %'d.\n" $TOTAL

# --- Puzzle: Part 2 ---
TOTAL=0
while read line; do
	parse_shape ${line:0:1}
	OP=$RESULT
	parse_result ${line:2:1}
	RES=$RESULT
	shape_to $OP $RES
	ME=$RESULT
	play $ME $OP
	TOTAL=$(( $TOTAL + $RESULT))
done < $INPUT_FILE
printf "Part 2. Total score: %'d.\n" $TOTAL

