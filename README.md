# OOPSLA 2025 Artifact: HieraSynth

This artifact provides the source code, benchmarks, and evaluation scripts for HieraSynth, a parallel framework for complete super-optimization,
described in our OOPSLA 2025 submission "HieraSynth: A Parallel Framework for Complete Super-Optimization with Hierarchical Space Decomposition".
The artifact is designed to allow for the full reproduction of all experimental results in the paper.
HieraSynth works by decomposing a large program synthesis search space into smaller, independent sub-problems that can be solved in parallel, enabling it to synthesize more complex optimal programs than prior work.

Due to hardware requirements, we understand that the reviewers may not have the resources and time to run the full experiments (See the Hardware Requirements section below), so we provide pre-computed raw data in the `results/` directory.
All the figures and tables can be reproduced without re-running the time-consuming experiments on a giant machine. We then provide scripts to allow the reproduction of some benchmarks on a standard multi-core machine. The smaller-scale experiments are able to support the major claims.

All claims made in the paper are supported by this artifact as follows.
We will provide the hardware requirements for each claim.

- **Claim 1: Benchmark Suite and Statistics (Section 7.1, Table 1)**
  - **Requirements**: Pre-computed, or 72-core full run
  - **Description**: The artifact contains the full benchmark suite, including 25 scalar and 26 vector problems. We show the statistics of the benchmark suite in this section.
  - **Note**: We have the 72-core requirement because the script is hardcoded to process the 72-core results. However, statistics does not depend on the number of cores being used.
- **Claim 2: Comparison with Brahma (Section 7.2, Fig. 9)**
  - **Requirements**: Pre-computed, 72-core full run, or 8-core partial run.
  - **Description**: We compare HieraSynth with its base synthesizer Brahma (implemented within HieraSynth framework).
- **Claim 3: Vector Code Synthesis (Section 7.3, Fig. 10)**
  - **Requirements**: Pre-computed, 72-core full run, or 8-core partial run.
  - **Description**: We show the effectiveness of HieraSynth in synthesizing vector code in this section.
- **Claim 4: The k-vs-n Tradeoff (Section 7.4, Fig. 12)**
  - **Requirements**: Pre-computed, or 72-core full run.
  - **Description**: We show the established trade-off between program length and the number of available components in existing systems, and compare HieraSynth with them.
  - **Note**: The script is hardcoded to process the 72-core results as this experiment is designed to show the capability boundary of HieraSynth, which cannot be achieved with just a few cores.
- **Claim 5: Case Study on `lt128` (Section 7.5, Fig. 13)**
  - **Requirements**: 72-core full run, or running the individual `lt128` benchmarks (not included in the script provided for 8-core run to save time).
  - **Description**: We show HieraSynth's ability to synthesize programs better than what human expert can identify in this section.
- **Claim 6: Parallel Speedup (Section 7.6, Fig. 14 & 15)**
  - **Requirements**: Pre-computed, 72-core full run, or 8-core partial run.
  - **Description**: We show the parallel speedup of HieraSynth in this section.

## Hardware Requirements

The hardware requirements depend on the desired scope of evaluation:

- **Plotting with Pre-computed Results**: Any machine capable of running Docker or Nix (e.g., a modern laptop) can be used to reproduce the figures from our pre-computed results.
- **Full Reproduction**: A machine with at least **72 CPU cores** and **100-200GB of RAM** is needed to replicate all experiments.
A full run takes several days across three such servers.
- **Partial Reproduction**: A modern multi-core machine with **8 CPU cores** and **24-32GB of RAM** is sufficient to validate the key findings. This takes a few hours.
A machine with 16GB of RAM can be used with an even smaller subset of the benchmarks.

The results in the [results](rvv-synthesizer/results) directory were obtained on three Rocky Linux 9.4 servers with dual Intel Xeon Platinum 8160M CPUs and 768GB of RAM.
The 8-core partial settings has been validated on a MacBook Pro with M4 Max and 48GB of RAM.

## Getting Started Guide

This guide covers setup and a basic sanity check, which should take 10~30 minutes depending on your network and hardware.

### Setup Options

You can set up the environment using either Docker or Nix.

#### Option 1: Using Docker

Run the following command to start a docker container with the artifact:

```bash
> docker run -it --name hierasynth-oopsla25-artifact lsrcz/hierasynth-oopsla25-artifact:latest
```

You will be dropped into a bash shell at `/hierasynth/rvv-synthesizer`, ready for evaluation.

#### Option 2: Using Nix

First, download the repository, then in the [rvv-synthesizer](rvv-synthesizer) folder, start the development shell:

```bash
> nix develop
```

Nix will build all dependencies with the exact versions we used.

### Sanity Check
From within the environment (Docker or Nix shell), run the test suite to verify the setup.
This will compile and run the synthesizer, and finish with all tests passed.

```bash
> stack test
```

Successfully completing this step confirms that the RVV synthesizer is functional and you are ready to proceed with the evaluation.

## Step-by-Step Instructions

We provide three evaluation paths. We recommend starting with Path 1 before deciding whether to proceed with Path 2 or 3 depending on your resources and time.

- **Path 1: Plot from Pre-computed Results (15 minutes)**: Use our pre-computed raw data in the `results/` directory to reproduce all figures and tables.
This is the fastest way to validate our claims.
- **Path 2: Partial Experiment Run (2 hours, 8 cores)**: Run a subset of experiments. The resulting figures will differ from the submission, but they should show the same trends to support the major claims.
- **Path 3: Full Experiment Run (1 week, 72 cores)**: Run all experiments to generate the results from scratch.

### Path 1: Validating Claims from Pre-computed Results (Recommended First Step)

All scripts should be run from the [rvv-synthesizer](rvv-synthesizer) directory.

#### Reproducing Table 1

- **Command**:
  ```bash
  > ./table_1.sh
  ```
- **Expected Runtime**: <10 seconds.
- **Expected Output**: This script generates the LaTeX source for Table 1 in [analysis/stats_table.tex] and prints it to the console, which summarizes the benchmark statistics.
- **Connection to the paper**: Table 1 is used to support Claim 1.

#### Reproducing Fig. 9, 10, 14, 15
- **Commands**:
  ```bash
  > ./fig_9_with_72_core.sh
  > ./fig_10_with_72_core.sh
  > ./fig_14_15_with_72_core.sh
  ```
- **Expected Runtime**: <30 seconds.
- **Expected Output**: Generates `rvv-synthesizer/analysis/figure_brahma.pdf` (Fig. 9), `rvv-synthesizer/analysis/figure_highway.pdf` (Fig. 10), `rvv-synthesizer/analysis/figure_par_best.pdf` (Fig. 14), and `rvv-synthesizer/analysis/figure_par_all.pdf` (Fig. 15).
- **Connection to the paper**: Fig. 9 supports Claim 2, Fig. 10 supports Claim 3, Fig. 14 and 15 support Claim 6.

#### Reproducing Fig. 1 and 12
- **Command**: `./fig_1_12.sh`.
- **Expected Runtime**: <10 seconds.
- **Expected Output**: Generates `rvv-synthesizer/analysis/figure_tradeoff_curve_selected.pdf` (Fig. 1) and `rvv-synthesizer/analysis/figure_tradeoff_curve.pdf` (Fig. 12).
- **Connection to the paper**: Fig. 12 supports Claim 4.

#### Reproducing Fig. 11
- **Command**: `./fig_11.sh`.
- **Expected Runtime**: <10 seconds.
- **Expected Output**:
  Generates `rvv-synthesizer/analysis/figure_highway_fail.pdf` (Fig. 11).
  This is an analysis of the failure case of vector synthesis.
  It is **NOT** reading any data from the `results/` directory,
  and we hardcoded the results to the script.
- **Connection to the paper**: Fig. 11 partially adds to Claim 3.

#### For Claim 5
Claim 5 isn't included in the pre-computed results.
The synthesized programs can be obtained by examining the execution logs of the `lt128` benchmarks.
We do not include them in the pre-computed results due to their sizes. We will provide instructions on how to obtain them later.

### Path 2 & 3: Reproducing Results by Running Experiments

#### Step 0: Clean Previous Results

To ensure plots are generated from your new runs, delete the pre-computed results.

```bash
> rm -rf results/
```

#### Step 1: Start the Task Queue Server
Our evaluation uses a Redis server to manage jobs.
- Command:
  ```bash
  > ./start_redis.sh
  ```
- Expected Output: Start a Redis server (default port 8934, password `rvv-synthesizer!`). You can configure the port and password in [rvv-synthesizer/redis.conf](rvv-synthesizer/redis.conf).

#### Step 2: Populate the Task Queue
In a new terminal, run a script based on your available hardware. Note that tmux is installed in the environment, so you can run multiple scripts side-by-side.

- For Path 2 (Partial, 8 cores): `./post_1_2_4_core_easy_jobs.sh`, `./post_8_core_easy_jobs.sh`.
  - Note: For RAM < 24GB, use the `./post_1_2_4_core_extra_easy_jobs.sh`, `./post_8_core_extra_easy_jobs.sh` variants.
  These will not include any `Lt128` tasks.
- For Path 3 (Full, 72 cores): `./post_all_jobs.sh`.

#### Step 3: Start the Runner
The runner fetches and executes jobs from the queue.

- For Path 2 (8 cores):
  ```bash
  > ./start_runner_8_core.sh localhost 8934
  ```
- For Path 3 (72 cores):
  ```bash
  > ./start_runner_72_core.sh localhost 8934
  ```
  (For multiple machines, replace `localhost` with the server IP.)

#### Step 4: Plot the Newly Generated Results
Once the runs are complete, use the plotting scripts from Path 1.
For 8-core runs, use the `_with_8_core.sh` variants.

### Verifying the Lt128 Case Study (Claim 5)

The synthesized programs for `lt128` are found in the execution logs.

#### Step 1: Run the benchmarks

The `lt128` benchmarks are included in the full run but excluded from the partial runs as they can be very slow when using a single core.

However, you can run them individually without the runner:

```bash
./scripts/bench/run_lt128_fixed.sh -j <n> -r 1 # Run Lt128.Fixed (with prologue) with n cores, repeat 1 time
./scripts/bench/run_lt128.sh -j <n> -r 1 # Run Lt128 (without prologue) with n cores, repeat 1 time
```

You should try to allocate as many cores as possible to reduce the runtime. Using 8 cores, this should take less than an hour.

#### Step 2: Examine the Logs:
The synthesized programs in our intermediate representation format can be found near the end of the log files located in
`logs/Lt128.<n>/.../notice.log` and `logs/Lt128.Fixed.<n>/.../notice.log`.

### Troubleshooting
The most likely cause of failure in a resource constrained environment is Out-of-Memory (OOM) error, which will **not** be identified and reported directly by the runner.
The error can be identified by finding a SIGKILL failure (-9 return code) for a solver in the log files.

This will cause plotting scripts to fail (often with a `NoneType` related error).
If this happens, you have two choices:

1. Manually run the individual benchmark without the runner.
2. Delete the corresponding empty/incomplete file from the `results/` directory. The plotting scripts will now ignore the missing data points.

## Reusability Guide
The core of this artifact is the HieraSynth framework, a reusable library for building super-optimizers.
The RVV synthesizer is an application built on this framework, demonstrating its reusability.
The framework is in the [hierasynth](hierasynth) folder, and is open-sourced under a BSD3 license at https://github.com/lsrcz/grisette-synth-lib (currently under a different name).
There are tutorial-style applications in the [hierasynth](hierasynth) directory
showing how to adapt the framework to a new ISA.
The [rvv-synthesizer/src/RVV/App.hs](rvv-synthesizer/src/RVV/App.hs) is the entry point of the RVV synthesizer, which shows how to configure the parallel reasoning engine for a complex application like RVV.

