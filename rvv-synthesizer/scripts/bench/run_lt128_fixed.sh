#!/usr/bin/env bash

usage() {
  echo "$0 usage:" && grep " .)\ #" "$0"
  exit 0
}
[ $# -eq 0 ] && usage

NUM_OF_CORES=1
USE_VL_MASK=""
NAME_POSTFIX=
SKETCH_SELECTOR=fixed_cmp_inferred_6
NUM_OF_RUNS=3

while getopts "j:hmo:r:" arg; do
  case $arg in
  j) # Number of cores
    NUM_OF_CORES=${OPTARG}
    ;;
  m) # Use vl mask
    USE_VL_MASK="--use-vl-mask=True"
    ;;
  o) # Use optimal sketch
    SKETCH_SELECTOR=fixed_cmp_optimal
    NAME_POSTFIX=".optimal"
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

echo "Number of cores: ${NUM_OF_CORES}"
echo "Compiling and warmup"
for _ in $(seq 1 2); do
  ./scripts/target/lt128.sh -j 1 --scheduler-timeout 1 >/dev/null 2>/dev/null
done

NAME=Lt128.Fixed${NAME_POSTFIX}.${NUM_OF_CORES}
if [[ -d logs/${NAME} ]]; then
  mkdir -p logs/"${NAME}".old
  mv logs/"${NAME}"/* logs/"${NAME}".old
fi
hyperfine -i -r${NUM_OF_RUNS} --export-json "results/${NAME}.json" \
  "./scripts/target/lt128.sh -j ${NUM_OF_CORES} --ir-func-name Lt128 --sketch-selector=${SKETCH_SELECTOR} --log-prog-name ${NAME} ${USE_VL_MASK}"
./scripts/analyze.py logs/"${NAME}"/*/results.csv >results/"${NAME}".csv

popd >>/dev/null || exit
