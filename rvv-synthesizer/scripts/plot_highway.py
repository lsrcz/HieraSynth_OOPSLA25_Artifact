#!/usr/bin/env python3

from typing import Dict, List, Tuple, Any, Optional
import matplotlib.pyplot as plt
from matplotlib import font_manager as fm
import os
import numpy as np
import csv
import argparse
from targets import (
    highway_targets,
    known_timeout_targets,
    vqsort_success_targets,
    vqsort_failed_targets,
)
from plot_util import (
    add_reference_lines,
    setup_plot_params,
    setup_y_axis,
    process_csv_row,
    save_plot,
    check_cost_improvement,
)

parser = argparse.ArgumentParser()

parser.add_argument(
    "-j",
    "--cores",
    type=int,
    help="Num of cores producing the data",
    default=72,
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
    "--output-file",
    type=str,
    help="Output file name",
    default=os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "..",
        "analysis",
        "figure_highway.pdf",
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
    default=6,
)

parser.add_argument(
    "--xaxis-rotation",
    type=int,
    help="X-axis rotation",
    default=30,
)

# Define colors for the main solution
MAIN_COLOR = "#1f77b4"  # Blue

# Define the solution type for highway (only main solution)
SOLUTION_TYPE = {
    "name": "main",
    "postfix": "",
    "best_label": "Time to known best solution",
    "optimal_label": "Time to prove optimal",
    "color": MAIN_COLOR,
}


def process_csv_file(
    target: str, cores: int, results_dir: str
) -> Tuple[Optional[float], Optional[float]]:
    """Process a CSV file for a specific target.

    Args:
        target: The target name
        cores: Number of cores used for the main solution
        results_dir: Directory containing the results files

    Returns:
        Tuple of (time_to_best, scheduler_elapsed_time)
    """
    filepath = os.path.join(results_dir, f"{target}.{cores}.csv")

    if not os.path.isfile(filepath):
        print(f"Warning: {filepath} not found")
        return 0, 0

    with open(filepath, "r") as f:
        reader = csv.DictReader(f)
        for row in reader:
            # Create a new dictionary with stripped column names
            clean_row = {k.strip(): v for k, v in row.items()}
            return process_csv_row(
                clean_row, target, known_timeout_targets, 3600
            )

    return 0, 0  # Default if file exists but no valid data found


def calculate_bar_positions(
    problems: List[str], bar_width: float
) -> Dict[str, np.ndarray]:
    """Calculate the positions for all bars in the plot.

    Args:
        problems: List of problem names
        bar_width: Width of each bar

    Returns:
        Dictionary of numpy arrays with positions for each bar type
    """
    positions = {
        "xaxis": [],
        "best": [],
        "all": [],
    }

    xoffset = 0

    for _ in problems:
        # Position for the current problem
        positions["xaxis"].append(xoffset + 0.5 * bar_width)

        # Best solution position
        positions["best"].append(xoffset)

        # Prove optimal position
        positions["all"].append(xoffset + bar_width)

        # Move to next problem
        group_spacing = 0.2  # Space between benchmarks
        xoffset += 2 * bar_width + group_spacing

    # Convert to numpy arrays for plotting
    return {k: np.array(v) for k, v in positions.items()}


def plot_bars(
    ax: Any,
    positions: Dict[str, np.ndarray],
    data: Dict[str, Dict[str, float]],
    bar_width: float,
) -> None:
    """Plot all bar groups with appropriate styling.

    Args:
        ax: Matplotlib axis to plot on
        positions: Dictionary of position arrays
        data: Nested dictionary of data values
        bar_width: Width of each bar
    """
    # Plot "best solution" bars
    ax.bar(
        positions["best"],
        list(data["best"].values()),
        bar_width,
        label=SOLUTION_TYPE["best_label"],
        color=SOLUTION_TYPE["color"],
    )

    # Plot "prove optimal" bars
    ax.bar(
        positions["all"],
        list(data["all"].values()),
        bar_width,
        label=SOLUTION_TYPE["optimal_label"],
        color=SOLUTION_TYPE["color"],
        hatch="//",
    )


if __name__ == "__main__":
    args = parser.parse_args()

    # Set plot parameters
    setup_plot_params()

    # Initialize data dictionaries
    data_dict = {
        "best": {},
        "all": {},
    }

    problem_label: Dict[str, str] = {}

    # Process highway targets
    for target in (
        highway_targets + vqsort_success_targets + vqsort_failed_targets
    ):
        # Process the main solution type only
        time_to_best, elapsed_time = process_csv_file(
            target, args.cores, args.results_dir
        )

        target0 = target.replace("_Ascending", "")
        target0 = target0.replace("_failure", "")
        target0 = target0.replace("ZeroExtendResizeBitCast", "Zero...Cast")
        target0 = target0.replace(".Fixed", ".WithPrologue")

        problem_label[target0] = ""

        if elapsed_time is None:
            problem_label[target0] += "*"
        if time_to_best is None:
            problem_label[target0] += "*"

        data_dict["best"][target0] = (
            time_to_best if time_to_best is not None else 3600
        )
        data_dict["all"][target0] = (
            elapsed_time if elapsed_time is not None else 3600
        )

        # Mark initial cost improvement
        # filepath = os.path.join(args.results_dir, f"{target}.{args.cores}.csv")
        # if check_cost_improvement(filepath):
        #     problem_label[target0] += "♦"

    problems = list(data_dict["best"].keys())
    fig = plt.figure(figsize=(args.figure_width, args.figure_height))
    ax = fig.add_subplot(1, 1, 1)  # Create a single subplot

    # Calculate bar positions
    bar_width = 0.4  # Wider bars since we only have two per benchmark
    positions = calculate_bar_positions(problems, bar_width)

    # Set x-axis labels
    xlabels = [f"{problem_label[p]}{p}" for p in problems]
    plt.xticks(
        positions["xaxis"],
        xlabels,
        rotation=args.xaxis_rotation,
        ha="right",
        fontproperties=fm.FontProperties(stretch="ultra-condensed", size=args.font_size),
    )
    # Set up y-axis
    setup_y_axis(ax, fontsize=args.font_size)

    # Plot all bars
    plot_bars(ax, positions, data_dict, bar_width)

    # Add reference lines
    add_reference_lines(ax)

    # Save the plot
    plt.legend(loc="upper left", fontsize=args.font_size, framealpha=args.frame_alpha)
    save_plot(args.output_file)
