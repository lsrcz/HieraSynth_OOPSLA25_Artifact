#! /usr/bin/env bash

SCRIPT_SOURCE=$(readlink -f -- "${BASH_SOURCE[0]}")
SCRIPT_DIR=$(dirname "${SCRIPT_SOURCE}")

pushd "${SCRIPT_DIR}/../.." >>/dev/null || exit

stack run zero_extend_resize_bit_cast -- \
  -r 50 \
  --scheduler-random-seed "$(cat /dev/urandom | tr -dc 0-9 | fold -w 8 | head -n 1)" \
  --poll-interval=0.2 \
  "$@" \
  --target-cost=-1 # -j 72 -t 300

popd >>/dev/null || exit
