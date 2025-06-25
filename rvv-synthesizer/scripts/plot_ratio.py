#!/usr/bin/env python3

from typing import Dict, List, Tuple, Optional
import matplotlib.pyplot as plt
from matplotlib.axes import Axes
import os
import numpy as np
from numpy.typing import NDArray
import argparse
import csv
from targets import (
    hacker_targets,
    highway_targets,
    known_timeout_targets,
)
from plot_util import (
    setup_plot_params,
    process_csv_row,
    save_plot,
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
        "figure_ratio.pdf",
    ),
)

parser.add_argument(
    "--include-optimality",
    action="store_true",
    help="Include 'time to prove optimality' ratios in the plot",
    default=False,
)

# Define colors for different ratio types
COLORS = {
    "best_ratio": "#1f77b4",  # Blue
    "optimal_ratio": "#FF7F0E",  # Orange
}


def process_csv_file(
    target: str, solution_type: str, cores: int, results_dir: str
) -> Tuple[Optional[float], Optional[float]]:
    """Process a CSV file for a specific target and solution type.

    Args:
        target: The target name
        solution_type: Solution type (empty for main, ".optimal" for optimal)
        cores: Number of cores used (only applied for main solution)
        results_dir: Directory containing the results files

    Returns:
        Tuple of (time_to_best, scheduler_elapsed_time)
    """
    # Use cores=1 for optimal solution, specified cores for main solution
    core_value = cores if solution_type == "" else 1
    filepath = os.path.join(
        results_dir, f"{target}{solution_type}.{core_value}.csv"
    )

    if not os.path.isfile(filepath):
        print(f"Warning: {filepath} not found")
        return None, None

    with open(filepath, "r") as f:
        reader = csv.DictReader(f)
        for row in reader:
            # Create a new dictionary with stripped column names
            clean_row = {k.strip(): v for k, v in row.items()}
            return process_csv_row(
                clean_row, target, known_timeout_targets, 3600
            )

    return None, None  # Default if file exists but no valid data found


def calculate_bar_positions(
    problems: List[str], bar_width: float, include_optimality: bool
) -> Dict[str, NDArray[np.float64]]:
    """Calculate the positions for all bars in the plot.

    Args:
        problems: List of problem names
        bar_width: Width of each bar
        include_optimality: Whether to include optimality bars

    Returns:
        Dictionary of numpy arrays with positions for each bar type
    """
    positions: Dict[str, List[float]] = {
        "xaxis": [],
        "best_ratio": [],
    }

    if include_optimality:
        positions["optimal_ratio"] = []

    xoffset = 0.0

    for _ in problems:
        # Position for the current problem
        if include_optimality:
            positions["xaxis"].append(xoffset + 0.5 * bar_width)
            # Position for best and optimal ratio bars
            positions["best_ratio"].append(xoffset)
            positions["optimal_ratio"].append(xoffset + bar_width)
            # Move to next problem with appropriate spacing
            problem_width = 2 * bar_width
        else:
            positions["xaxis"].append(xoffset)
            # Only best ratio bar
            positions["best_ratio"].append(xoffset)
            # Move to next problem with appropriate spacing
            problem_width = bar_width

        problem_spacing = 0.6  # Space between problems
        xoffset += problem_width + problem_spacing

    # Convert to numpy arrays for plotting
    return {k: np.array(v, dtype=np.float64) for k, v in positions.items()}


def plot_bars(
    ax: Axes,
    positions: Dict[str, NDArray[np.float64]],
    data: Dict[str, Dict[str, float]],
    bar_width: float,
    include_optimality: bool,
) -> None:
    """Plot all bar groups with appropriate styling.

    Args:
        ax: Matplotlib axis to plot on
        positions: Dictionary of position arrays
        data: Nested dictionary of data values
        bar_width: Width of each bar
        include_optimality: Whether to include optimality bars
    """
    # Plot "best solution" ratio bar
    ax.bar(
        positions["best_ratio"],
        list(data["best_ratio"].values()),
        bar_width,
        label="Ratio of time to best (PolySynth/optimal)",
        color=COLORS["best_ratio"],
    )

    # Plot "prove optimal" ratio bar if requested
    if include_optimality and "optimal_ratio" in data:
        ax.bar(
            positions["optimal_ratio"],
            list(data["optimal_ratio"].values()),
            bar_width,
            label="Ratio of time to prove optimality (PolySynth/optimal)",
            color=COLORS["optimal_ratio"],
            hatch="//",
        )


def setup_y_axis_ratio(ax: Axes):
    """Set up the y-axis for ratio plot.

    Args:
        ax: Matplotlib axis to configure
    """
    ax.set_ylabel("Ratio (PolySynth/optimal)", fontsize=17)
    # ax.set_ylim(0, 1.1)  # Set y-axis limits from 0 to 1.1 to show full range
    ax.grid(True, linestyle="--", alpha=0.7)
    ax.set_yscale("log")


if __name__ == "__main__":
    args = parser.parse_args()

    # Initialize data dictionaries
    data_dict: Dict[str, Dict[str, float]] = {
        "best_ratio": {},
    }

    if args.include_optimality:
        data_dict["optimal_ratio"] = {}

    problem_label: Dict[str, str] = {}

    # Process only hacker targets
    for target in hacker_targets + highway_targets:
        problem_label[target] = ""

        # Get data for main (PolySynth) solution
        main_time_to_best, main_elapsed_time = process_csv_file(
            target, "", args.cores, args.results_dir
        )

        # Get data for optimal solution
        optimal_time_to_best, optimal_elapsed_time = process_csv_file(
            target, ".optimal", 1, args.results_dir
        )

        # Calculate ratios
        # For the case where PolySynth timed out, set the ratio as 0
        if main_time_to_best is None or main_time_to_best >= 3600:
            data_dict["best_ratio"][target] = 0
        elif optimal_time_to_best is None:
            data_dict["best_ratio"][target] = 0
        else:
            data_dict["best_ratio"][target] = (
                main_time_to_best / optimal_time_to_best
            )

        # Only calculate optimality ratios if requested
        if args.include_optimality:
            if main_elapsed_time is None or main_elapsed_time >= 3600:
                data_dict["optimal_ratio"][target] = 0
            elif optimal_elapsed_time is None:
                data_dict["optimal_ratio"][target] = 0
            else:
                data_dict["optimal_ratio"][target] = (
                    main_elapsed_time / optimal_elapsed_time
                )

        # Add special markers for main solution
        if main_elapsed_time is None:
            problem_label[target] += "*"

        # No longer using hacker_need_extra_targets
        # if target in hacker_need_extra_targets:
        #     problem_label[target] += "★"

        # No longer using check_cost_improvement
        # Mark initial cost improvement (using main solution data)
        # filepath = os.path.join(args.results_dir, f"{target}.{args.cores}.csv")
        # if check_cost_improvement(filepath):
        #     problem_label[target] += "▴"

    # Set plot parameters
    setup_plot_params()

    problems = list(data_dict["best_ratio"].keys())
    fig = plt.figure(figsize=(25, 7))
    ax = fig.add_subplot(1, 1, 1)  # Create a single subplot

    # Calculate bar positions
    bar_width = 0.4
    positions = calculate_bar_positions(
        problems, bar_width, args.include_optimality
    )

    # Set x-axis labels
    xlabels = [f"{problem_label[p]}{p}" for p in problems]
    plt.xticks(positions["xaxis"], xlabels, rotation=40, ha="right")

    # Set up y-axis for ratio plot
    setup_y_axis_ratio(ax)

    # Plot all bars
    plot_bars(ax, positions, data_dict, bar_width, args.include_optimality)

    # Add a horizontal line at y=1 to indicate where optimal = PolySynth
    # ax.axhline(
    #     y=1.0,
    #     color="red",
    #     linestyle="-",
    #     lw=1,
    #     alpha=0.7,
    #     label="Equal performance (ratio=1)",
    # )

    # Save the plot
    plt.legend(loc="upper left", fontsize=25, framealpha=0.6)
    save_plot(args.output_file)
