#! /usr/bin/env bash

SCRIPT_SOURCE=$(readlink -f -- "${BASH_SOURCE[0]}")
SCRIPT_DIR=$(dirname "${SCRIPT_SOURCE}")

IR_FUNC_NAME_BASE=$1
shift 1
BITS=$1
shift 1
NUM_INFERRED_COMPONENTS=
TARGET_COST_ARG=
SCHEDULER_TIMEOUT_ARG=

case $IR_FUNC_NAME_BASE in
'PrevValue_Ascending')
  case $BITS in
  128)
    NUM_INFERRED_COMPONENTS=5
    ;;
  64)
    NUM_INFERRED_COMPONENTS=2
    ;;
  32)
    NUM_INFERRED_COMPONENTS=2
    ;;
  *)
    echo "Not supported by PrevValue_Ascending"
    exit 1
    ;;
  esac
	;;
'SwapAdjacentPairs')
  case $BITS in
  64)
    NUM_INFERRED_COMPONENTS=6
    ;;
  32)
    NUM_INFERRED_COMPONENTS=6
    ;;
  *)
    echo "Not supported by SwapAdjacentPairs"
    exit 1
    ;;
  esac
  ;;
'SwapAdjacentQuads')
  case $BITS in
  64)
    NUM_INFERRED_COMPONENTS=4
    ;;
  32)
    NUM_INFERRED_COMPONENTS=6
    TARGET_COST_ARG="--target-cost=10"
    ;;
  *)
    echo "Not supported by SwapAdjacentQuads"
    exit 1
    ;;
  esac
  ;;
'SortPairsDistance1_Ascending_failure')
  IR_FUNC_NAME_BASE=SortPairsDistance1_Ascending
  case $BITS in
  128)
    echo "Likely cannot synthesize, timeout in 1 hour"
    NUM_INFERRED_COMPONENTS=8 # Cannot synthesize
    SCHEDULER_TIMEOUT_ARG="--scheduler-timeout=3620"
    ;;
  *)
    echo "Not supported by SortPairsDistance1_Ascending_failure"
    exit 1
    ;;
  esac
  ;;
'SortPairsDistance1_Ascending')
  case $BITS in
  64)
    NUM_INFERRED_COMPONENTS=6
    ;;
  32)
    NUM_INFERRED_COMPONENTS=7
    TARGET_COST_ARG="--target-cost=20"
    ;;
  *)
    echo "Not supported by SortPairsDistance1_Ascending"
    exit 1
    ;;
  esac
  ;;
'SortPairsDistance4_Ascending')
  case $BITS in
  64)
    NUM_INFERRED_COMPONENTS=6
    ;;
  *)
    echo "Not supported by SortPairsDistance4_Ascending"
    exit 1
    ;;
  esac
  ;;
'SortPairsDistance4_Ascending_failure')
  IR_FUNC_NAME_BASE=SortPairsDistance4_Ascending
  case $BITS in
  32)
    echo "Likely cannot synthesize, timeout in 1 hour"
    NUM_INFERRED_COMPONENTS=8 # Cannot synthesize
    SCHEDULER_TIMEOUT_ARG="--scheduler-timeout=3620"
    ;;
  *)
    echo "Not supported by SortPairsDistance4_Ascending_failure"
    exit 1
    ;;
  esac
esac

echo "$IR_FUNC_NAME_BASE.$BITS with $NUM_INFERRED_COMPONENTS components"

pushd "${SCRIPT_DIR}/../.." >> /dev/null || exit

stack run vqsort -- \
  --ir-func-name "${IR_FUNC_NAME_BASE}_$BITS" \
  --poll-interval=0.2 \
	--scheduler-random-seed "$(cat /dev/urandom | tr -dc 0-9 | fold -w 8 | head -n 1)" \
  --num-inferred-components "$NUM_INFERRED_COMPONENTS" \
  $TARGET_COST_ARG \
  $SCHEDULER_TIMEOUT_ARG \
  "$@"

popd >> /dev/null || exit
