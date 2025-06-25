#!/usr/bin/env bash

usage() {
  echo "$0 usage:" && grep " .)\ #" "$0"
  exit 0
}
[ $# -eq 0 ] && usage

NUM_OF_CORES=1
NUM_OF_RUNS=3
USE_VL_MASK=""
USE_OPTIMAL_SKETCH=
NAME_POSTFIX=

while getopts "n:j:l:u:hmo:r:" arg; do
  case $arg in
  j) # Number of cores
    NUM_OF_CORES=${OPTARG}
    ;;
  n) # Number of runs
    NUM_OF_RUNS=${OPTARG}
    ;;
  r) # Number of runs
    NUM_OF_RUNS=${OPTARG}
    ;;
  m) # Use vl mask
    USE_VL_MASK="--use-vl-mask=True"
    ;;
  o) # Use optimal sketch
    USE_OPTIMAL_SKETCH="--use-optimal-sketch"
    NAME_POSTFIX=".optimal"
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

echo "Number of cores: ${NUM_OF_CORES}"

echo "Compiling and warmup"
for _ in $(seq 1 2); do
  ./scripts/target/insert_lane.sh -j 1 --scheduler-timeout 1 >/dev/null 2>/dev/null
done

list="InsertLane"

for func in $list; do
  NAME=${func}${NAME_POSTFIX}.${NUM_OF_CORES}
  if [[ -d logs/${NAME} ]]; then
    mkdir -p logs/"${NAME}".old
    mv logs/"${NAME}"/* logs/"${NAME}".old
  fi
  hyperfine -i -r"${NUM_OF_RUNS}" --export-json "results/${NAME}.json" \
    "./scripts/target/insert_lane.sh -j ${NUM_OF_CORES} --ir-func-name ${func} --log-prog-name ${NAME} ${USE_VL_MASK} ${USE_OPTIMAL_SKETCH}"
  ./scripts/analyze.py logs/"${NAME}"/*/results.csv >results/"${NAME}".csv
done

popd >>/dev/null || exit
