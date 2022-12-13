#!/bin/bash

LANGS=(bash java javascript python rust)
OK=":white_check_mark:"
NO=" "


echo '# My Advent of Code Solutions

[Advent of Code](https://adventofcode.com/) is an annual set of Christmas-themed computer programming challenges that follow an Advent calendar. It has been running since 2015.

The repository is organized by year and then by day. Each day, there is a copy of the two parts puzzle and their solutions in various programming languages.
'


base=$(pwd)
for year in $(ls -rd */); do
	year=${year%"/"}
	echo "## $year"
	echo -n "| Day |"
	for lang in "${LANGS[@]}"; do
		echo -n " $lang |"
	done
	echo ""
	echo -n "|:---:|"
	for lang in "${LANGS[@]}"; do
		echo -n ":---:|"
	done
	echo ""

	pushd $year > /dev/null
	for day in $(ls -d */); do
		day=${day%"/"}
		echo -n "| [$day]($year/$day) |"
		for lang in "${LANGS[@]}"; do
			dir="$base/$year/$day/$lang"
			if [ -d "$dir" ] && [ -n "$(ls -A $dir)" ]; then
				echo -n "[$OK]($year/$day/$lang)"
			else
				echo -n "${NO}"
			fi
			echo -n "|"
		done
		echo ""
	done
	popd > /dev/null
done
