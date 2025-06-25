#! /usr/bin/env bash

usage() {
  echo "$0 usage:" && grep " .)\ #" "$0"
  exit 0
}
[ $# -eq 0 ] && usage

IR_FUNC_NAME_BASE=$1
shift 1

NUM_OF_CORES=1
USE_VL_MASK=""
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
    echo "Not implemented"
    exit 1
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

echo "Number of cores: ${NUM_OF_CORES}"
mkdir -p results
echo "Compiling and warmup"
for _ in $(seq 1 2); do
  ./scripts/target/vqsort.sh -j 1 --num-inferred-components 3 --scheduler-timeout 1 >/dev/null 2>/dev/null
done

case $IR_FUNC_NAME_BASE in
'PrevValue_Ascending')
  BITS=(128 64 32)
  ;;
'SwapAdjacentPairs')
  BITS=(64 32)
  ;;
'SwapAdjacentQuads')
  BITS=(32)
  ;;
'SortPairsDistance1_Ascending')
  BITS=(64 32)
  ;;
'SortPairsDistance1_Ascending_failure')
  BITS=(128)
  ;;
'SortPairsDistance4_Ascending')
  BITS=(64)
  ;;
'SortPairsDistance4_Ascending_failure')
  BITS=(32)
  ;;
esac

for B in "${BITS[@]}"; do
  NAME=${IR_FUNC_NAME_BASE}_${B}.${NUM_OF_CORES}
  if [[ -d logs/${NAME} ]]; then
    mkdir -p logs/"${NAME}".old
    mv logs/"${NAME}"/* logs/"${NAME}".old
  fi
  hyperfine -i -r${NUM_OF_RUNS} --export-json "results/${NAME}.json" \
    "./scripts/target/vqsort.sh ${IR_FUNC_NAME_BASE} ${B} -j ${NUM_OF_CORES} --log-prog-name ${NAME} ${USE_VL_MASK} ${USE_OPTIMAL_SKETCH}"
  ./scripts/analyze.py logs/"${NAME}"/*/results.csv >results/"${NAME}".csv
done

popd >>/dev/null || exit
