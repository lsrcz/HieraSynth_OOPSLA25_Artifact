#! /usr/bin/env bash

# Add --runs 1 to only run once

python scripts/runner/job_generator.py --cores 1,2,4,8,16,32,48,72 | \
  python scripts/runner/publisher.py --stdin --redis-host localhost \
  --redis-port 8934 --redis-password rvv-synthesizer!
