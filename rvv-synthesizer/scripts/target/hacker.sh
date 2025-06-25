#! /usr/bin/env bash

SCRIPT_SOURCE=$(readlink -f -- "${BASH_SOURCE[0]}")
SCRIPT_DIR=$(dirname "${SCRIPT_SOURCE}")

VARIANT=$1
shift 1
IR_FUNC_NAME=$1
shift 1
declare -a EXTRA_ARGS=()

case $VARIANT in
'inferred')
	case $IR_FUNC_NAME in
	'p01') INFERRED_INST=2 ;;
	'p02') INFERRED_INST=2 ;;
	'p03') INFERRED_INST=2 ;;
	'p04') INFERRED_INST=2 ;;
	'p05') INFERRED_INST=2 ;;
	'p06') INFERRED_INST=2 ;;
	'p07') INFERRED_INST=2 ;;
	'p08') INFERRED_INST=2 ;;
	'p09') INFERRED_INST=2 ;;
	'p10') INFERRED_INST=4 ;;
	'p11') INFERRED_INST=2 ;;
	'p12') INFERRED_INST=3 ;;
	'p13') INFERRED_INST=2 ;;
	'p14') INFERRED_INST=4 ;;
	'p15') INFERRED_INST=4 ;;
	'p16') INFERRED_INST=1 ;;
	'p17') INFERRED_INST=4 ;;
	'p18') INFERRED_INST=3 ;;
	'p19') INFERRED_INST=6 ;;
	'p20')
		INFERRED_INST=6
		EXTRA_ARGS+=(--has-div)
		;;
	'p21')
		INFERRED_INST=7
		EXTRA_ARGS+=(--target-cost=7)
		;;
	'p22') INFERRED_INST=3 ;;
	'p23')
		INFERRED_INST=1
		;;
	'p24') INFERRED_INST=5 ;;
	'p25')
		INFERRED_INST=1
		EXTRA_ARGS+=(--has-mul)
		;;
	esac
	EXTRA_ARGS+=(--num-of-inferred-inst="${INFERRED_INST}")
	;;
'stdlib')
	EXTRA_ARGS+=(--use-stdlib --timeout 3600 --scheduler-timeout 3620)
	;;
'original')
	EXTRA_ARGS+=(--use-original --timeout 3600 --scheduler-timeout 3620)
	;;
'optimal')
	EXTRA_ARGS+=(--use-optimal --timeout 3600 --scheduler-timeout 3620)
	;;
'stdlib_fixed_imm')
	EXTRA_ARGS+=(--use-stdlib --use-fixed-imm --timeout 3600 --scheduler-timeout 3620)
	;;
'original_fixed_imm')
	EXTRA_ARGS+=(--use-original --use-fixed-imm --timeout 3600 --scheduler-timeout 3620 --vconsts=vlen128)
	;;
'optimal_fixed_imm')
	EXTRA_ARGS+=(--use-optimal --use-fixed-imm --timeout 3600 --scheduler-timeout 3620 --vconsts=vlen128)
	;;
esac

# Add vconsts=vlen128 for p23 and p25 for stdlib, original, and optimal variants
# Skip adding for -fixed-imm variants as they already have it
if [[ "$IR_FUNC_NAME" == "p23" || "$IR_FUNC_NAME" == "p25" ]]; then
    if [[ "$VARIANT" == "stdlib" || "$VARIANT" == "inferred" || "$VARIANT" == "original" || "$VARIANT" == "optimal" ]]; then
        EXTRA_ARGS+=(--vconsts=vlen128)
    fi
fi

pushd "${SCRIPT_DIR}/../.." >> /dev/null || exit

stack run hacker -- \
	--full-scalar-cost \
	--initial-cost-offset=1 \
	--ir-func-name "$IR_FUNC_NAME" \
	--poll-interval=0.2 \
	--scheduler-random-seed "$(cat /dev/urandom | tr -dc 0-9 | fold -w 8 | head -n 1)" \
	"${EXTRA_ARGS[@]}" \
	"$@"

popd >> /dev/null || exit
