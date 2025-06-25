#! /usr/bin/env bash

python scripts/runner/job_generator.py --extra-easy --runs 1 --cores 8 | \
  python scripts/runner/publisher.py --stdin --redis-host localhost \
  --redis-port 8934 --redis-password rvv-synthesizer!
