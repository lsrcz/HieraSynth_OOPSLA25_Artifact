#! /usr/bin/env bash

REDIS_HOST=$1
REDIS_PORT=$2

python scripts/runner/runner.py --total-cores 72 --redis-host $REDIS_HOST \
  --redis-port $REDIS_PORT --redis-password rvv-synthesizer!
