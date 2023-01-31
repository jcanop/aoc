#!/bin/bash

LANGS=(bash c java javascript python rust)
OK=":white_check_mark:"
WARNING=":warning:"
NO=" "


echo '# My Advent of Code Solutions

[Advent of Code](https://adventofcode.com/) is an annual set of Christmas-themed computer programming challenges that follow an Advent calendar. It has been running since 2015.

The repository is organized by year and then by day. Each day, there is a copy of the two parts puzzle and their solutions in various programming languages.

You can directly access the selected source code by clicking each green button on the table or read the puzzle description by clicking the puzzle name.
'


base=$(pwd)
for year in $(ls -rd 2*/); do
	year=${year%"/"}
	echo "## $year"
	echo -n "|     |"
	for lang in "${LANGS[@]}"; do
		echo -n " $lang |"
	done
	echo -n " comments |"
	echo ""
	echo -n "|:----|"
	for lang in "${LANGS[@]}"; do
		echo -n ":---:|"
	done
	echo -n ":----|"
	echo ""

	pushd $year > /dev/null
	for day in $(ls -d */); do
		day=${day%"/"}
		file="$base/$year/$day/README.md"
		if [ -f "$file" ]; then
			title=$(grep -e "^# Day" $file)
			echo -n "| [${title:2}]($year/$day) |"
		else
			echo -n "| [Day $day]($year/$day) |"
		fi
		for lang in "${LANGS[@]}"; do
			dir="$base/$year/$day/$lang"
			if [ -d "$dir" ] && [ -n "$(ls -A $dir)" ]; then
				if [ -f "$dir/warning.txt" ]; then
					tp=$(cat $dir/warning.txt)
					echo -n "[$WARNING]($year/$day/$lang \"$tp\")"
				else
					echo -n "[$OK]($year/$day/$lang)"
				fi
			else
				echo -n "${NO}"
			fi
			echo -n "|"
		done
		file="$base/$year/$day/comments.md"
		if [ -f "$file" ]; then
			comments=$(cat $file)
			echo -n " $comments "
		else
			echo -n "${NO}"
		fi
		echo -n "|"
		echo ""
	done
	popd > /dev/null
done

echo '## Compile and Run the Code
### C
```
# Compile (GCC is used but should work with others compilers)
$ make build

# Run
$ make run
```

### Java
```
# Compile
$ javac *.java -d build -Xlint

# Run
$ java -cp build Main
```

### Javascript
```
# Run
$ node main.mjs
```

### Python
```
# Run
$ python3 main.py
```

### Rust
```
# Compile with Cargo
$ cargo build

# Run with Cargo
$ cargo run

# Compile without Cargo
$ rustc main.rs

# Run without Cargo
$ ./main
```
'

