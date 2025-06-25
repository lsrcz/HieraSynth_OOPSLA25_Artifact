#! /usr/bin/env bash

SCRIPT_SOURCE=$(readlink -f -- "${BASH_SOURCE[0]}")
SCRIPT_DIR=$(dirname "${SCRIPT_SOURCE}")

pushd "${SCRIPT_DIR}/../.." >>/dev/null || exit

stack run lt128 -- \
  -r 50 \
  --scheduler-random-seed "$(cat /dev/urandom | tr -dc 0-9 | fold -w 8 | head -n 1)" \
  --poll-interval=0.2 \
  "$@"

popd >>/dev/null || exit
