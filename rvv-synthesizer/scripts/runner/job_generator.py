#!/usr/bin/env python3

import argparse
import json
import sys
import os

# -----------------------
# Command Line Argument Parsing
# -----------------------
parser = argparse.ArgumentParser(
    description="Generate benchmark jobs for the distributed scheduler"
)
parser.add_argument(
    "--easy",
    action="store_true",
    help="Generate jobs only for easy benchmarks (skip slow problems)",
)
parser.add_argument(
    "--extra-easy",
    action="store_true",
    help="Generate jobs only for extra easy benchmarks (skip slow problems and ones requiring larger RAM)",
)
parser.add_argument(
    "--runs",
    type=int,
    default=3,
    help="Number of runs for each benchmark (default: 3)",
)
parser.add_argument(
    "--cores",
    type=str,
    default="1,2,4,8,16,24,48,72",
    help="Comma-separated list of core counts to generate (default: 1,2,4,8,16,24,48,72)",
)
parser.add_argument(
    "--benchmarks",
    type=str,
    default="all",
    help="Comma-separated list of benchmarks to generate",
)
parser.add_argument(
    "--output", type=str, default="-", help="Output file (default: stdout)"
)
script_paths = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
    "bench",
)
parser.add_argument(
    "--prefix",
    type=str,
    default=script_paths,
    help=f"Prefix path for benchmark scripts (default: {script_paths})",
)
args = parser.parse_args()
if args.extra_easy:
    args.easy = True


# -----------------------
# Parse Core Composition
# -----------------------
def parse_cores(cores_str):
    """Parse the comma-separated list of core counts"""
    try:
        cores = [int(c.strip()) for c in cores_str.split(",")]
        return sorted(list(set(cores)))  # Remove duplicates and sort
    except ValueError:
        sys.exit(1)


# -----------------------
# Benchmark Definitions and Job Generation
# -----------------------
def generate_standard_benchmark_jobs(benchmark_name, cores, prefix):
    """Generate jobs for benchmarks that follow the standard pattern with variable core counts"""
    jobs = []
    script_name = benchmark_name

    for core_count in cores:
        job_id = f"{benchmark_name}-j{core_count}"
        command = f"{prefix}/run_{script_name}.sh -j {core_count} -r {args.runs}"

        jobs.append({"id": job_id, "command": command, "cores": core_count})
    return jobs


def generate_vqsort_jobs(benchmark_name, cores, prefix):
    jobs = []
    for core_count in cores:
        job_id = f"{benchmark_name}-j{core_count}"
        command = f"{prefix}/run_vqsort.sh {benchmark_name} -j {core_count} -r {args.runs}"

        jobs.append({"id": job_id, "command": command, "cores": core_count})
    return jobs


def generate_vqsort_72core_failure_jobs(benchmark_name, prefix):
    job_id = f"{benchmark_name}-j72"
    command = f"{prefix}/run_vqsort.sh {benchmark_name} -j 72 -r {args.runs}"

    return [{"id": job_id, "command": command, "cores": 72}]

RUN_HACKER_SCRIPT = "run_hacker.sh" if not args.easy else "run_hacker_easy.sh"

def generate_hacker_inferred_jobs(cores, prefix):
    """Generate jobs for hacker_inferred benchmark"""
    jobs = []
    for core_count in cores:
        job_id = f"hacker_inferred-j{core_count}"
        command = f"{prefix}/{RUN_HACKER_SCRIPT} inferred -j {core_count} -r {args.runs}"

        jobs.append({"id": job_id, "command": command, "cores": core_count})
    return jobs


def generate_hacker_single_core_jobs(benchmark_type, prefix):
    """Generate jobs for benchmarks that only run with a single core"""
    job_id = f"{benchmark_type}-j1"
    command = (
        f"{prefix}/{RUN_HACKER_SCRIPT} {benchmark_type.replace('hacker_', '')} -j 1 -r {args.runs}"
    )

    return [{"id": job_id, "command": command, "cores": 1}]


# -----------------------
# Main Job Generation Logic
# -----------------------
all_vector_benchmarks = {
    "add_sub",
    "concat",
    "insert_lane",
    "lt128",
    "lt128_fixed",
    "min128",
    "min128_fixed",
    "mul_even_odd",
    "odd_even_blocks",
    "zero_extend_resize_bit_cast",
}
all_vqsort_success_benchmarks = {
    "PrevValue_Ascending",
    "SwapAdjacentPairs",
    "SwapAdjacentQuads",
    "SortPairsDistance1_Ascending",
    "SortPairsDistance4_Ascending",
}
all_vqsort_failure_benchmarks = {
    "SortPairsDistance1_Ascending_failure",
    "SortPairsDistance4_Ascending_failure",
}
all_hacker_inferred_benchmarks = {"hacker_inferred"}
all_hacker_single_core_benchmarks = {
    # "hacker_stdlib_fixed_imm",
    "hacker_stdlib",
    # "hacker_original",
    "hacker_optimal",
}
hard_benchmarks = {
    "min128",
    "min128_fixed",
    "lt128",
    "odd_even_blocks",
    "mul_even_odd",
    "SwapAdjacentPairs",
    "SwapAdjacentQuads",
    "SortPairsDistance1_Ascending",
    "SortPairsDistance1_Ascending_failure",
    "SortPairsDistance4_Ascending_failure",
}
medium_benchmarks = {
    "lt128_fixed",
}


def generate_jobs(benchmark_list, cores_list, prefix):
    """Generate jobs for the specified benchmarks with the given core counts"""
    all_jobs = []

    for benchmark in benchmark_list:
        if benchmark in all_vector_benchmarks:
            all_jobs.extend(
                generate_standard_benchmark_jobs(benchmark, cores_list, prefix)
            )

        elif benchmark in all_hacker_inferred_benchmarks:
            all_jobs.extend(generate_hacker_inferred_jobs(cores_list, prefix))

        elif benchmark in all_hacker_single_core_benchmarks:
            # These benchmarks always use a single core, regardless of the core composition
            if 1 in cores_list:
                all_jobs.extend(
                    generate_hacker_single_core_jobs(benchmark, prefix)
                )
        elif benchmark in all_vqsort_success_benchmarks:
            all_jobs.extend(generate_vqsort_jobs(benchmark, cores_list, prefix))
        elif benchmark in all_vqsort_failure_benchmarks:
            if 72 in cores_list:
                all_jobs.extend(
                    generate_vqsort_72core_failure_jobs(benchmark, prefix)
                )

        else:
            raise ValueError(f"Unknown benchmark type: {benchmark}")
    return all_jobs


# -----------------------
# Output Handling
# -----------------------
def write_jobs_to_output(jobs, output_path):
    """Write the generated jobs to the specified output"""
    jobs_json = json.dumps(jobs, indent=2)

    if output_path == "-":
        # Write to stdout
        print(jobs_json)
    else:
        # Write to file
        try:
            with open(output_path, "w") as f:
                f.write(jobs_json)
        except IOError as e:
            sys.exit(1)


def main():
    # Parse command line arguments
    cores = parse_cores(args.cores)
    benchmarks = {b.strip() for b in args.benchmarks.split(",")}
    if "all" in benchmarks:
        benchmarks |= (
            all_vector_benchmarks
            | all_hacker_inferred_benchmarks
            | all_hacker_single_core_benchmarks
            | all_vqsort_success_benchmarks
            | all_vqsort_failure_benchmarks
        )
        benchmarks -= {"all"}
    if "all_vqsort_success" in benchmarks:
        benchmarks |= all_vqsort_success_benchmarks
        benchmarks -= {"all_vqsort_success"}
    if "all_vqsort_failure" in benchmarks:
        benchmarks |= all_vqsort_failure_benchmarks
        benchmarks -= {"all_vqsort_failure"}
    if "all_vqsort" in benchmarks:
        benchmarks |= (
            all_vqsort_success_benchmarks | all_vqsort_failure_benchmarks
        )
        benchmarks -= {"all_vqsort"}

    if args.easy:
        benchmarks -= hard_benchmarks
    if args.extra_easy:
        benchmarks -= medium_benchmarks

    # Generate jobs
    jobs = generate_jobs(benchmarks, cores, args.prefix)

    # Write jobs to output
    write_jobs_to_output(jobs, args.output)


if __name__ == "__main__":
    main()
