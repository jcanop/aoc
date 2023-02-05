#!/bin/bash
#
#  This script can test all de code to verify that everything is correct. It
#  also accepts command line arguments if you only want to test by year, day,
#  or language and define an output file name to store test stats.
#
#  While the solutions should work on any UNIX-type system (Linux, MacOS, BSD,
#  etc.) except for C, this script is only tested on Debian Linux.

# --- Constants --
LANGS=(bash c java javascript python rust)
WORK_BASE_DIR=/dev/shm
COMMONS_DIR="commons"
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ANSWERS=$(cat "$SCRIPT_DIR/test.json")

RED="\033[1;31m"
GREEN="\033[1;32m"
YELLOW="\033[1;33m"
WHITE="\033[1;37m"
NORMAL="\033[0m"

OK="OK"
NOOP="NOOP"
WARNING="??"
ERROR="ERROR"

# --- Check if the base directory exists ---
if [ ! -d "$WORK_BASE_DIR" ]; then
	echo "Working directory not found!"
	exit 1
fi

# --- Compile the code ---
function compile {
	local lang=$1
	if  [ "$lang" == "bash" ] || \
		[ "$lang" == "javascript" ] || \
		[ "$lang" == "python" ]; then
		printf $NOOP
		return 0
	elif [ "$lang" == "c" ]; then
		make -s build
	elif [ "$lang" == "java" ]; then
		javac *.java -d build
	elif [ "$lang" == "rust" ]; then
		if [ -f "Cargo.toml" ]; then
			cargo build --release --quiet
		else
			rustc -C opt-level=3 main.rs
		fi
	fi
	if [ $? -eq 0 ]; then
		printf $OK
	else
		printf $ERROR
	fi
	return $?
}

# --- Execute the code ---
function execute {
	local lang=$1
	if [ "$lang" == "bash" ]; then
		echo -n $(./main.sh)
	elif [ "$lang" == "c" ]; then
		echo -n $(make run)
	elif [ "$lang" == "java" ]; then
		echo -n $(java -cp build Main)
	elif [ "$lang" == "javascript" ]; then
		echo -n $(node main.mjs)
	elif [ "$lang" == "python" ]; then
		echo -n $(python3 main.py)
	elif [ "$lang" == "rust" ]; then
		if [ -f "Cargo.toml" ]; then
			echo -n $(cargo run --release --quiet)
		else
			echo -n $(./main)
		fi
	fi
	return $?
}

# --- Checks the output and validate that is correct ---
function test_result {
	local output=$(echo "$1" | sed -r 's/,//g')
	local answers=$2
	if [ "$answers" == "null" ]; then
		printf $WARNING
		return 1
	else
		for answer in $(echo "$answers" | jq -r ".[]"); do
			if [[ ! $output == *"$answer"* ]]; then
				printf $ERROR
				return 2
			fi
		done
	fi
	printf $OK
}

# --- Prints the result ---
function print_result {
	if [ "$1" == "$OK" ]; then
		printf "$GREEN$OK$NORMAL   "
	elif [ "$1" == "$NOOP" ]; then
		printf "$GREEN$NOOP$NORMAL "
	elif [ "$1" == "$WARNING" ]; then
		printf "$YELLOW$WARNING$NORMAL   "
	elif [ "$1" == "$ERROR" ]; then
		printf "$RED$ERROR$NORMAL "
	else
		printf $1
	fi
}

# --- Init the JSON stats file ---
function init_json {
	local now=$(date +%s)
	local os=$(uname -r)
	local arch=$(lscpu -J | jq '.lscpu[] | select(.field=="Architecture:")' | jq -r .data)
	local model=$(lscpu -J | jq '.lscpu[] | select(.field=="Model name:")' | jq -r .data)
	local cores=$(lscpu -J | jq '.lscpu[] | select(.field=="CPU(s):")' | jq -r .data)
	local mem=$(free -h | head -n 2 | tail -n 1 | awk '{print $2}')
	local v_bash=$(bash --version | head -n 1)
	local v_java=$(java --version | grep Runtime)
	local v_node=$(node --version)
	local v_python=$(python3 --version)
	local v_rust=$(rustc --version)

	echo $(jq --null-input \
		--argjson start $now \
		--argjson tests "{}" \
		--arg os $os \
		--arg arch $arch \
		--arg model "$model" \
		--argjson cores $cores \
		--arg mem $mem \
		--arg v_bash "$v_bash" \
		--arg v_java "$v_java" \
		--arg v_node "$v_node" \
		--arg v_python "$v_python" \
		--arg v_rust "$v_rust" \
		'{ "start": $start, "end": 0, "tests": $tests, vm: { "os":$os, "cpu": { "arch": $arch, "model": $model, "cores": $cores }, "ram": $mem }, "versions": { "bash": $v_bash, "java": $v_java, "node": $v_node, "python": $v_python, "rust": $v_rust }}')
}

# --- Prints the help ---
function print_help {
	echo "Script that tests all the code and generate the stats file."
	echo ""
	echo "Usage: $0 [options]"
	echo ""
	echo "Options:"
	echo "  -d <DAY>     Filter by days"
	echo "  -h           Print help"
	echo "  -j           Prints the JSON stats"
	echo "  -l <LANG>    Filter by languanges"
	echo "  -o <FILE>    Output filename"
	echo "  -y <YEAR>    Filter by years"
	echo ""
	echo "Example: $0 -y 2022 -d 01,02 -l java"
	echo ""
}

# --- Command line arguments ---
while getopts "d:l:o:y:hj" opt; do
	case "${opt}" in
		d) FILTER_DAY=${OPTARG};;
		j) PRINT_JSON="Y";;
		l) FILTER_LANG=${OPTARG};;
		o) OUTPUT_FILE=${OPTARG};;
		y) FILTER_YEAR=${OPTARG};;
		*) print_help; exit 0;;
	esac
done

# --- Checks arguments ---
if [ "$FILTER_YEAR" != "" ] && [[ ! $FILTER_YEAR =~ ^[0-9]{4}(,[0-9]{4})*$ ]]; then
		printf "\n** Invalid filter years: $FILTER_YEAR **\n\n"
		print_help
		exit 1
fi
if [ "$FILTER_DAY" != "" ]; then
	if [[ ! $FILTER_DAY =~ ^[0-9]{2}([-,][0-9]{2})*$ ]]; then
		printf "\n** Invalid filter days: $FILTER_DAY **\n\n"
		print_help
		exit 1
	fi
	filter=""
	while [[ $FILTER_DAY =~ (([0-9]+-)?[0-9]+) ]]; do
		token="${BASH_REMATCH[1]}"
		if [[ ${#token} -eq 5 ]]; then
			a=$(( 10#${token:0:2} ))
			b=$(( 10#${token:3} ))
			for (( i=$a; i <=$b; i++ )); do
				filter="$filter,"
				if [[ $i -le 9 ]]; then
					filter=$filter'0'
				fi
				filter="$filter$i"
			done
		else
			filter="$filter,$token"
		fi
		FILTER_DAY=${FILTER_DAY##*${BASH_REMATCH[1]}}
	done
	FILTER_DAY=${filter:1}
fi

# --- Prepare working temporal directories ---
if [ -d "$WORK_BASE_DIR/$COMMONS_DIR" ]; then
	rm -rf "$WORK_BASE_DIR/$COMMONS_DIR"
fi
cp -r "$SCRIPT_DIR/../$COMMONS_DIR" "$WORK_BASE_DIR"

# --- Main script ---
JSON=$(init_json)

pushd $SCRIPT_DIR > /dev/null
cd ..
base=$(pwd)

for year in $(ls -rd 2*/); do
	year=${year%"/"}
	if [ "$FILTER_YEAR" != "" ] && [[ ! ",$FILTER_YEAR," == *",$year,"* ]]; then
		continue;
	fi

	pushd $year > /dev/null
	for day in $(ls -d */); do
		day=${day%"/"}
		if [ "$FILTER_DAY" != "" ] && [[ ! ",$FILTER_DAY," == *",$day,"* ]]; then
			continue;
		fi

		echo "$year-$day"

		# --- JSON ---
		JSON=$(echo $JSON | jq ".tests += {\"$year-$day\": {}}")

		# --- Create a temp working dir and copy the code files ---
		WORK_DIR=`mktemp -d -p "$WORK_BASE_DIR"`
		cp -r $day "$WORK_DIR"
		pushd "$WORK_DIR/$day" > /dev/null

		# --- Search for the expected answers ---
		answers=$(echo $ANSWERS | jq -c ".answers.\"$year-$day\"")
		for lang in "${LANGS[@]}"; do
			if [ "$FILTER_LANG" != "" ] && [[ ! ",$FILTER_LANG," == *",$lang,"* ]]; then
				continue;
			fi

			if [ -d $lang ] && [ ! -f "$lang/warning.txt" ]; then
				printf "* %-12s " $lang
				pushd $lang > /dev/null

				# --- Compile ---
				printf "Compile: "
				compile_start=$(date +%s)
				result=$(compile $lang)
				compile_end=$(date +%s)
				compile_ecode=$?
				print_result $result
				cjson=$(jq --null-input \
					--arg result "$result" \
					--argjson compile_start $compile_start \
					--argjson compile_end $compile_end \
					'{"result": $result, "start": $compile_start, "end": $compile_end}')

				# --- Execute ---
				printf "    Execute: "
				if [ $compile_ecode -eq 0 ]; then
					execute_start=$(date +%s)
					output=$(execute $lang)
					execute_end=$(date +%s)
					if [ $? -ne 0 ]; then
						result=$ERROR
						print_result $ERROR
					else
						result=$OK
						seconds=$(($execute_end - $execute_start))
						duration=$(date -ud "@$seconds" +'%M:%S')
						printf $GREEN
						printf $duration
						printf $NORMAL
					fi
				else
					execute_start=$(date +%s)
					result=$NOOP
					print_result $result
					execute_end=$(date +%s)
				fi
				ejson=$(jq --null-input \
					--arg result "$result" \
					--argjson execute_start $execute_start \
					--argjson execute_end $execute_end \
					'{"result": $result, "start": $execute_start, "end": $execute_end}')


				# --- Test ---
				printf "    Test: "
				result=$(test_result "$output" "$answers")
				if [ "$result" == "$OK" ] && [ "$lang" == "c" ]; then
					valgrind --tool=memcheck --leak-check=full -s --error-exitcode=1 build/main > /dev/null 2>&1
					if [ $? -ne 0 ]; then
						result=$ERROR
					fi
				fi
				print_result $result
				echo ""

				# --- JSON ---
				tjson="{\"compile\": $cjson, \"execute\": $ejson, \"test\": \"$result\"}"
				JSON=$(echo $JSON | jq ".tests.\"$year-$day\" += {\"$lang\": $tjson}")

				popd > /dev/null
			fi
		done
		echo ""

		# --- Delete the temp dir ---
		popd > /dev/null
		rm -rf "$WORK_DIR"
	done
	popd > /dev/null
done
popd > /dev/null

if [ -d "$WORK_BASE_DIR/$COMMONS_DIR" ]; then
	rm -rf "$WORK_BASE_DIR/$COMMONS_DIR"
fi

# --- JSON ---
now=$(date +%s)
JSON=$(echo $JSON | jq ".end |= $now")
if [ "$PRINT_JSON" == "Y" ]; then
	echo $JSON
fi
if [ "$OUTPUT_FILE" != "" ]; then
	echo $JSON > $OUTPUT_FILE
fi

# --- Print Results ---
echo ""
echo "------------------------------------------------------------------"
c1=0
printf $WHITE
printf "%-12s" "Language"
printf $GREEN
printf "  %16s" "OK"
printf $YELLOW
printf "  %16s" "WARNING"
printf $RED
printf "  %16s" "ERROR"
printf $NORMAL
echo ""
echo "------------------------------------------------------------------"
c1=0
c2=0
c3=0
for lang in "${LANGS[@]}"; do
	r1=$(echo $JSON | jq ".tests[].$lang.test" | grep "$OK" | wc -l)
	r2=$(echo $JSON | jq ".tests[].$lang.test" | grep "$WARNING" | wc -l)
	r3=$(echo $JSON | jq ".tests[].$lang.test" | grep "$ERROR" | wc -l)
	printf "%-12s  %16d  %16d  %16d\n" $lang $r1 $r2 $r3
	c1=$(($c1 + r1))
	c2=$(($c2 + r2))
	c3=$(($c3 + r3))
done
echo "------------------------------------------------------------------"
printf "%-12s  %16d  %16d  %16d\n" "total" $c1 $c2 $c3
echo "------------------------------------------------------------------"

# --- Test time ---
stime=$(echo $JSON | jq .start)
etime=$(echo $JSON | jq .end)
seconds=$(( $etime - $stime ))
ptime=$(date -ud "@$seconds" +'%H hours %M minutes %S seconds')
printf "%-12s  %52s\n\n" "time" "$ptime"


# --- Makes sure to cleanup before exit ---
function cleanup {
	if [ -d "$WORK_DIR" ]; then
		rm -rf "$WORK_DIR"
	fi
}

trap cleanup EXIT
