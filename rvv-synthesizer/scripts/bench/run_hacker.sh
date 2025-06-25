#!/usr/bin/env bash

usage() {
	echo "$0 usage:" && grep " .)\ #" "$0"
	exit 0
}
[ $# -eq 0 ] && usage

NAME_POSTFIX=
VARIANT=$1
shift 1
case $VARIANT in
'inferred') NAME_POSTFIX= ;;
'stdlib') NAME_POSTFIX=.std ;;
'stdlib_fixed_imm') NAME_POSTFIX=.std.fixed.imm ;;
'original') NAME_POSTFIX=.original ;;
'original_fixed_imm') NAME_POSTFIX=.original.fixed.imm ;;
'optimal') NAME_POSTFIX=.optimal ;;
'optimal_fixed_imm') NAME_POSTFIX=.optimal.fixed.imm ;;
esac
NUM_OF_RUNS=3
NUM_OF_CORES=1
PROBLEM_LOWER=1
PROBLEM_UPPER=25

while getopts "j:l:u:hn:e:r:" arg; do
	case $arg in
	n) # Number of runs
		NUM_OF_RUNS=${OPTARG}
		;;
	j) # Number of cores
		NUM_OF_CORES=${OPTARG}
		;;
	l) # Lower bound for problem number
		PROBLEM_LOWER=${OPTARG}
		;;
	u) # Upper bound for problem number
		PROBLEM_UPPER=${OPTARG}
		;;
	e) # Exact bound
		PROBLEM_LOWER=${OPTARG}
		PROBLEM_UPPER=${OPTARG}
		;;
	r) # Number of runs
		NUM_OF_RUNS=${OPTARG}
		;;
	h | *) # Display help.
		usage
		;;
	esac
done
SCRIPT_SOURCE=$(readlink -f -- "${BASH_SOURCE[0]}")
SCRIPT_DIR=$(dirname "${SCRIPT_SOURCE}")

pushd "${SCRIPT_DIR}/../.." >>/dev/null || exit

mkdir -p results

echo "Variant: ${VARIANT}"
echo "Number of cores: ${NUM_OF_CORES}"
echo "Lower bound for problem number ${PROBLEM_LOWER}"
echo "Upper bound for problem number ${PROBLEM_UPPER}"

echo "Compiling and warmup"
for _ in $(seq 1 2); do
	./scripts/target/hacker.sh p01 -j 1 >/dev/null 2>/dev/null
done

for i in $(seq -f %02g "${PROBLEM_LOWER}" "${PROBLEM_UPPER}"); do
	NAME=p${i}${NAME_POSTFIX}.${NUM_OF_CORES}
	if [[ -d logs/${NAME} ]]; then
		mkdir -p "logs/${NAME}.old"
		mv logs/"${NAME}"/* "logs/${NAME}.old"
	fi
	hyperfine -i -r"${NUM_OF_RUNS}" --export-json "results/${NAME}.json" \
		"./scripts/target/hacker.sh ${VARIANT} p${i} -j ${NUM_OF_CORES} --log-prog-name ${NAME}"
	python3 ./scripts/analyze.py "$(pwd)"/logs/"${NAME}"/*/results.csv >"results/${NAME}.csv"
done

popd >>/dev/null || exit
