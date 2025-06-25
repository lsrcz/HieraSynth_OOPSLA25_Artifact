#!/usr/bin/env python3

from typing import Dict, List, Tuple, Any
import matplotlib.pyplot as plt
from matplotlib import font_manager as fm
import os
import numpy as np
import csv
from targets import (
    hacker_targets,
    highway_targets,
    known_timeout_targets,
    vqsort_success_targets,
)
import argparse
from plot_util import (
    time_to_float,
    setup_plot_params,
    save_plot,
)

# Create parser with common options and add parallel-specific arguments
parser = argparse.ArgumentParser()
parser.add_argument(
    "-t",
    "--targets",
    nargs="+",
    help="List of targets to plot",
    default=hacker_targets + highway_targets + vqsort_success_targets,
)
parser.add_argument(
    "--results-dir",
    type=str,
    help="Directory containing the results files",
    default=os.path.join(
        os.path.dirname(os.path.abspath(__file__)), "..", "results"
    ),
)
parser.add_argument(
    "-c",
    "--cores",
    type=int,
    nargs="+",
    help="Number of cores to plot",
    default=[1, 2, 4, 8, 16, 24, 48, 72],
)
parser.add_argument(
    "--best-output",
    type=str,
    help="Output file name for 'best solution' plot",
    default=os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "..",
        "analysis",
        "figure_par_best.pdf",
    ),
)
parser.add_argument(
    "--all-output",
    type=str,
    help="Output file name for 'all solution' plot",
    default=os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "..",
        "analysis",
        "figure_par_all.pdf",
    ),
)

parser.add_argument(
    "--font-size",
    type=int,
    help="Font size",
    default=25,
)

parser.add_argument(
    "--frame-alpha",
    type=float,
    help="Frame alpha",
    default=0.6,
)

parser.add_argument(
    "--figure-width",
    type=float,
    help="Figure width",
    default=25,
)

parser.add_argument(
    "--figure-height",
    type=float,
    help="Figure height",
    default=7,
)

parser.add_argument(
    "--xaxis-rotation",
    type=int,
    help="X-axis rotation",
    default=20,
)


def get_next_available_core(
    core: int,
    core_list: List[int],
    data: Dict[str, Dict[int, float]],
    target: str,
) -> int:
    """Find the next available core with data for scaling.

    Args:
        core: The core count we need data for
        core_list: List of all core counts in ascending order
        data: Dictionary containing available data
        target: The target benchmark

    Returns:
        The next core count that has data available
    """
    for next_core in core_list:
        if next_core > core and next_core in data[target]:
            return next_core
    return -1  # No larger core count available


def get_prev_available_core(
    core: int,
    core_list: List[int],
    data: Dict[str, Dict[int, float]],
    target: str,
) -> int:
    """Find the previous available core with data for scaling.

    Args:
        core: The core count we need data for
        core_list: List of all core counts in descending order
        data: Dictionary containing available data
        target: The target benchmark

    Returns:
        The previous core count that has data available
    """
    for prev_core in reversed(core_list[: core_list.index(core)]):
        if prev_core in data[target]:
            return prev_core
    return -1  # No smaller core count available


def read_data_with_scaling(
    targets: List[str], cores: List[int], results_dir: str, use_best_time: bool
) -> Dict[str, Dict[int, float]]:
    """Read timing data from CSV files with handling for missing data.

    Args:
        targets: List of target benchmarks
        cores: List of core counts to process
        results_dir: Directory containing results
        use_best_time: If True, use time_to_best, otherwise use total time

    Returns:
        Dictionary of timing data for each target and core count
    """
    data: Dict[str, Dict[int, float]] = {}

    # First pass: read all available data
    for target0 in targets:
        if not use_best_time and target0 in known_timeout_targets:
            print(
                f"Warning: target {target0} is in known_timeout_targets, skipping"
            )
            continue

        target = target0.replace("_Ascending", "")
        target = target.replace("_failure", "")
        target = target.replace(".Fixed", ".WithPrologue")

        data[target] = {}
        has_some_data = False

        # Read all available data files
        for core in cores:
            filepath = os.path.join(results_dir, f"{target0}.{core}.csv")
            if not os.path.isfile(filepath):
                print(f"Warning: file not found {filepath}")
                continue

            with open(filepath, "r") as f:
                reader = csv.DictReader(f)
                for row in reader:
                    clean_row = {k.strip(): v for k, v in row.items()}
                    if use_best_time:
                        data[target][core] = time_to_float(
                            clean_row["time_to_best_since_scheduler_start"]
                        )
                    else:
                        data[target][core] = time_to_float(
                            clean_row["scheduler_elapsed_time"]
                        )
                    has_some_data = True
                    break  # Just use the first row

        if not has_some_data:
            print(
                f"Warning: no data found for target {target}, removing from analysis"
            )
            del data[target]

    # Second pass: fill in missing data by scaling
    sorted_cores = sorted(cores)
    for target in list(data.keys()):
        for core in sorted_cores:
            if core not in data[target]:
                # Try to find the next larger core count with data
                next_core = get_next_available_core(
                    core, sorted_cores, data, target
                )

                if next_core > 0:
                    # Scale down the time (larger core = less time)
                    scaled_time = data[target][next_core] * (next_core / core)
                    data[target][core] = scaled_time
                    print(
                        f"Warning: Scaled data for {target} at {core} cores from {next_core} cores"
                    )
                    continue

                # If no larger core available, try using a smaller core count
                prev_core = get_prev_available_core(
                    core, sorted_cores, data, target
                )

                if prev_core > 0:
                    # Scale up the time (smaller core = more time)
                    scaled_time = data[target][prev_core] * (prev_core / core)
                    data[target][core] = scaled_time
                    print(
                        f"Warning: Scaled data for {target} at {core} cores from {prev_core} cores"
                    )
                    continue

                # If we get here, we couldn't find any scaling source
                print(
                    f"Error: Could not scale data for {target} at {core} cores, insufficient data"
                )

    return data


def get_valid_targets(
    data: Dict[str, Dict[int, float]],
    cores: List[int],
    minimum_core: int,
    min_time_threshold: float = 10.0,
) -> List[str]:
    """Filter targets to those with valid data and sort by runtime.

    Args:
        data: Dictionary of timing data
        cores: List of core counts
        minimum_core: The minimum core count to use as baseline
        min_time_threshold: Minimum time threshold for valid targets

    Returns:
        Sorted list of valid targets
    """
    valid_targets_with_time: List[Tuple[str, float]] = []

    for target in data:
        if minimum_core not in data[target]:
            print(
                f"Warning: no data for {target} at minimum core count {minimum_core}"
            )
            continue

        min_core_time = data[target][minimum_core]
        if min_core_time < min_time_threshold:
            print(
                f"Warning: {target} runtime at {minimum_core} cores is below threshold ({min_core_time} < {min_time_threshold})"
            )
            continue

        valid_targets_with_time.append((target, min_core_time))

    # Sort targets by runtime at minimum core count
    valid_targets_with_time.sort(key=lambda x: x[1])
    valid_targets = [target for target, _ in valid_targets_with_time]

    return valid_targets


def compute_speedup_ratio(
    data: Dict[str, Dict[int, float]], targets: List[str], minimum_core: int
) -> Dict[str, Dict[int, float]]:
    """Compute speedup ratios relative to the minimum core count.

    Args:
        data: Dictionary of timing data
        targets: List of target benchmarks
        minimum_core: The minimum core count to use as baseline

    Returns:
        Dictionary of speedup ratios
    """
    ratio: Dict[str, Dict[int, float]] = {}

    for target in targets:
        minimum_core_time = data[target][minimum_core]
        ratio[target] = {}

        for core in data[target]:
            # Speedup ratio = t1/tn where t1 is time on min cores and tn is time on n cores
            ratio[target][core] = minimum_core_time / data[target][core]

    return ratio


def plot_parallel_performance(
    speedup_ratio: Dict[str, Dict[int, float]],
    targets: List[str],
    cores: List[int],
    output_file: str,
) -> None:
    """Create a bar chart showing parallel speedup for each target.

    Args:
        speedup_ratio: Dictionary of speedup ratios
        targets: List of target benchmarks
        cores: List of core counts
        output_file: Path to save the output figure
    """
    # Set up plot parameters
    setup_plot_params()
    plt.figure(figsize=(args.figure_width, args.figure_height))
    ax = plt.gca()

    # Set up axes
    xaxis = np.arange(len(targets))
    plt.xticks(
        xaxis,
        targets,
        rotation=args.xaxis_rotation,
        ha="right",
        fontproperties=fm.FontProperties(stretch="ultra-condensed", size=args.font_size),
    )
    ax.set_yscale("log")

    # Calculate maximum result for setting y-axis limits
    max_result = round(
        max(
            [
                speedup_ratio[t][c]
                for t in targets
                for c in cores
                if c in speedup_ratio[t]
            ]
        ),
        1,
    )

    # Set up y-axis ticks and reference lines
    ticks = [1, 2, 4, 8, 16, 24, 48]
    ticks.append(max_result)
    # if max_result < 72:
    #     ticks.append(max_result)
    # else:
    #     ax.axhline(y=72, color="grey", linestyle="--", lw=1)
    #     ticks.append(72)
    #     ticks.append(max_result)

    plt.yticks(ticks, [str(t) for t in ticks], fontsize=args.font_size)

    # Add horizontal reference lines for common speedup values
    for tick in ticks:
        ax.axhline(y=tick, color="grey", linestyle="--", lw=1)

    # Calculate bar widths and positions
    all_widths = 0.8
    each_width = all_widths / len(cores)
    start = each_width / 2 * (1 - len(cores))

    # Plot bars for each core count
    for i, core in enumerate(cores):
        values = [
            speedup_ratio[t][core] if core in speedup_ratio[t] else 0
            for t in targets
        ]
        ax.bar(
            xaxis + start + i * each_width,
            values,
            each_width,
            label=f"{core} cores",
        )

    # Add y-axis label
    ax.set_ylabel("Speedup (relative to 1 core)", fontsize=args.font_size)

    # Save the plot
    plt.legend(loc="upper left", fontsize=args.font_size, framealpha=args.frame_alpha)
    save_plot(output_file)


if __name__ == "__main__":
    args = parser.parse_args()

    # Sort core counts
    args.cores.sort()
    minimum_core = min(args.cores)

    # Define the metrics to process
    metrics = [
        {
            "name": "best solution",
            "use_best_time": True,
            "output_file": args.best_output,
            "data": None,
            "valid_targets": None,
            "speedup_ratio": None,
        },
        {
            "name": "total solution time",
            "use_best_time": False,
            "output_file": args.all_output,
            "data": None,
            "valid_targets": None,
            "speedup_ratio": None,
        },
    ]

    # Process each metric type completely before moving to the next
    for metric in metrics:
        print(f"Processing '{metric['name']}' data...")

        # Read data with scaling for this metric
        metric["data"] = read_data_with_scaling(
            args.targets, args.cores, args.results_dir, metric["use_best_time"]
        )

        # Get valid targets for this metric
        metric["valid_targets"] = get_valid_targets(
            metric["data"], args.cores, minimum_core
        )
        print(
            f"Valid targets for '{metric['name']}' plot: {len(metric['valid_targets'])}"
        )

        # Compute speedup ratios for this metric
        metric["speedup_ratio"] = compute_speedup_ratio(
            metric["data"], metric["valid_targets"], minimum_core
        )

        # Create plot for this metric
        print(f"Creating '{metric['name']}' plot: {metric['output_file']}")
        plot_parallel_performance(
            metric["speedup_ratio"],
            metric["valid_targets"],
            args.cores,
            metric["output_file"],
        )

    # Print summary table in LaTeX format
    print("\nSummary Table (LaTeX format):")
    print("\\begin{tabular}{|l|c|c|}")
    print("\\hline")
    print("Subject & Best Time Speedup & Total Time Speedup \\\\")
    print("\\hline")

    # Calculate speedup from maximum to minimum cores
    max_core = max(args.cores)
    print(
        f"${max_core}/{minimum_core}$ (ideal) & {max_core/minimum_core:.1f} & {max_core/minimum_core:.1f} \\\\"
    )
    print("\\hline")

    best_metric = metrics[0]
    all_metric = metrics[1]

    # Show speedup for each target (if available in both datasets)
    for target in best_metric["valid_targets"]:
        best_speedup = (
            best_metric["speedup_ratio"][target][max_core]
            if max_core in best_metric["speedup_ratio"][target]
            else "N/A"
        )
        all_speedup = "N/A"

        if (
            target in all_metric["valid_targets"]
            and max_core in all_metric["speedup_ratio"][target]
        ):
            all_speedup = all_metric["speedup_ratio"][target][max_core]

        print(
            f"{target} & {best_speedup:.2f} & {all_speedup if all_speedup == 'N/A' else f'{all_speedup:.2f}'} \\\\"
        )

    print("\\hline")
    print("\\end{tabular}")
