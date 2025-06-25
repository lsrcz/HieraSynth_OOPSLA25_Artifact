import os
import csv
import sys
import argparse
from collections import defaultdict
from typing import Dict, DefaultDict, Set

from targets import highway_targets, hacker_targets


def get_benchmark_name(filename: str) -> str:
    # Remove the .72.csv suffix and return the benchmark name
    return filename[:-7]


def get_all_targets() -> Set[str]:
    """Get all targets from targets.py, excluding known_timeout_targets and hacker_need_extra_targets."""
    return set(highway_targets + hacker_targets)


def process_results(data_dir: str) -> None:
    metrics_of_interest = [
        "scheduler_elapsed_time",
        "time_to_best_since_scheduler_start",
        "time_to_best",
    ]

    # Dictionary to store results for each benchmark
    benchmark_results: DefaultDict[str, Dict[str, float]] = defaultdict(dict)
    processed_benchmarks: Set[str] = set()
    missing_benchmarks: Set[str] = set()

    # Get all expected targets
    all_targets = get_all_targets()

    # Process all .72.csv files
    for filename in os.listdir(data_dir):
        if filename.endswith(".72.csv"):
            benchmark_name = get_benchmark_name(filename)
            file_path = os.path.join(data_dir, filename)
            
            # Check if file is empty
            if os.path.getsize(file_path) == 0:
                continue  # Skip empty files, they'll be counted as missing
            
            with open(file_path, "r") as f:
                # Read the CSV with space-stripped fieldnames
                reader = csv.DictReader((line.replace(" ", "") for line in f))
                rows = list(reader)
                if not rows:  # If no data rows
                    continue  # Skip files with header but no data
                    
                processed_benchmarks.add(benchmark_name)
                for metric in metrics_of_interest:
                    benchmark_results[metric][benchmark_name] = float(rows[0][metric])

    # Find missing benchmarks
    missing_benchmarks = all_targets - processed_benchmarks

    # Sort benchmarks alphabetically (use all_targets to maintain consistent order)
    benchmarks = sorted(list(all_targets))

    # Write the output CSV
    with open("summary.csv", "w", newline="") as f:
        writer = csv.writer(f)

        # Write header
        writer.writerow(["metric"] + benchmarks)

        # Write data for each metric
        for metric in metrics_of_interest:
            row = [metric]
            for benchmark in benchmarks:
                if benchmark in missing_benchmarks:
                    row.append("N/A")  # Mark missing data
                else:
                    row.append(str(benchmark_results[metric][benchmark]))
            writer.writerow(row)

    # Report missing benchmarks if any
    if missing_benchmarks:
        print("Warning: The following benchmarks are missing:", file=sys.stderr)
        for benchmark in sorted(missing_benchmarks):
            print(f"  - {benchmark}", file=sys.stderr)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Process benchmark results from .72.csv files."
    )
    parser.add_argument(
        "data_dir", help="Directory containing the .72.csv files"
    )
    args = parser.parse_args()

    if not os.path.isdir(args.data_dir):
        print(f"Error: {args.data_dir} is not a directory", file=sys.stderr)
        sys.exit(1)

    process_results(args.data_dir)


if __name__ == "__main__":
    main()
