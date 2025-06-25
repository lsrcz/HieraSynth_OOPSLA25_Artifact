#!/usr/bin/env python3

from typing import Dict, List, Tuple, Optional
import matplotlib.pyplot as plt
from matplotlib import font_manager as fm
from matplotlib.axes import Axes
import os
import numpy as np
from numpy.typing import NDArray
import argparse
import csv
from targets import (
    hacker_targets,
    hacker_need_extra_targets,
    known_timeout_targets,
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
        "figure_brahma.pdf",
    ),
)

parser.add_argument(
    "--include-original",
    action="store_true",
    help="Include 'original' variant in the plot",
    default=False,
)

parser.add_argument(
    "--include-optimal",
    action="store_true",
    help="Include 'optimal' variant in the plot",
    default=False,
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

# Define colors for different solution types - these can be customized
COLORS = {
    "main": "#1f77b4",  # Blue
    "original": "#FFC500",  # Yellow
    "stdlib": "#FF7F0E",  # Orange
    "optimal": "#FF0000",  # Red
}

# Define solution types and their properties
ALL_SOLUTION_TYPES = [
    {
        "name": "main",
        "postfix": "",
        "best_label": "Time to best (HieraSynth)",
        "optimal_label": "Time to prove optimality (HieraSynth)",
        "color": COLORS["main"],
    },
    {
        "name": "original",
        "postfix": ".original",
        "best_label": "Time to best (original)",
        "optimal_label": "Time to prove optimality (original)",
        "color": COLORS["original"],
    },
    {
        "name": "stdlib",
        "postfix": ".std",
        "best_label": "Time to best (Brahma stdlib with optimal extension)",
        "optimal_label": "Time to prove optimality (Brahma stdlib with optimal extension)",
        "color": COLORS["stdlib"],
    },
    {
        "name": "optimal",
        "postfix": ".optimal",
        "best_label": "Time to best (optimal)",
        "optimal_label": "Time to prove optimality (optimal)",
        "color": COLORS["optimal"],
    },
]


def process_csv_file(
    target: str, solution_type: Dict[str, str], cores: int, results_dir: str
) -> Tuple[Optional[float], Optional[float]]:
    """Process a CSV file for a specific target and solution type.

    Args:
        target: The target name
        solution_type: Dictionary containing solution type information
        cores: Number of cores used (only applied for main solution)
        results_dir: Directory containing the results files

    Returns:
        Tuple of (time_to_best, scheduler_elapsed_time)
    """
    postfix = solution_type["postfix"]
    # Use cores=1 for all solutions except the main solution (empty postfix)
    core_value = cores if postfix == "" else 1
    filepath = os.path.join(results_dir, f"{target}{postfix}.{core_value}.csv")

    if not os.path.isfile(filepath):
        print(f"Warning: {filepath} not found")
        raise FileNotFoundError(f"File {filepath} not found")

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
    problems: List[str], solution_types: List[Dict[str, str]], bar_width: float
) -> Dict[str, NDArray[np.float64]]:
    """Calculate the positions for all bars in the plot.

    Args:
        problems: List of problem names
        solution_types: List of solution types to plot
        bar_width: Width of each bar

    Returns:
        Dictionary of numpy arrays with positions for each bar type
    """
    positions: Dict[str, List[float]] = {
        "xaxis": [],
    }

    # Initialize positions dict entries for each solution type
    for sol_type in solution_types:
        name = sol_type["name"]
        positions[f"{name}_best"] = []
        positions[f"{name}_all"] = []

    xoffset = 0.0
    solution_count = len(solution_types)

    for _ in problems:
        # Position for the current problem
        positions["xaxis"].append(
            xoffset + ((solution_count - 0.5) * bar_width)
        )

        # For each solution type, add positions for both "best" and "all" bars
        for idx, sol_type in enumerate(solution_types):
            name = sol_type["name"]

            # Group each solution type together (best and optimal side by side for each solution)
            variant_offset = xoffset + (idx * 2 * bar_width)
            positions[f"{name}_best"].append(variant_offset)
            positions[f"{name}_all"].append(variant_offset + bar_width)

        # Move to next problem with appropriate spacing
        problem_width = 2 * solution_count * bar_width
        problem_spacing = 0.6  # Space between problems
        xoffset += problem_width + problem_spacing

    # Convert to numpy arrays for plotting
    return {k: np.array(v, dtype=np.float64) for k, v in positions.items()}


def plot_bars(
    ax: Axes,
    positions: Dict[str, NDArray[np.float64]],
    data: Dict[str, Dict[str, float]],
    solution_types: List[Dict[str, str]],
    bar_width: float,
) -> None:
    """Plot all bar groups with appropriate styling.

    Args:
        ax: Matplotlib axis to plot on
        positions: Dictionary of position arrays
        data: Nested dictionary of data values
        solution_types: List of solution type configurations
        bar_width: Width of each bar
    """
    # Plot solution type groups (both best and optimal for each solution type)
    for sol_type in solution_types:
        name = sol_type["name"]
        # Plot "best solution" bar
        ax.bar(
            positions[f"{name}_best"],
            list(data[f"{name}_best"].values()),
            bar_width,
            label=sol_type["best_label"],
            color=sol_type["color"],
        )
        # Plot "prove optimal" bar
        ax.bar(
            positions[f"{name}_all"],
            list(data[f"{name}_all"].values()),
            bar_width,
            label=sol_type["optimal_label"],
            color=sol_type["color"],
            hatch="//",
        )


if __name__ == "__main__":
    args = parser.parse_args()

    # Set plot parameters
    setup_plot_params()

    # Select which solution types to include based on arguments
    SOLUTION_TYPES = [
        s
        for s in ALL_SOLUTION_TYPES
        if (s["name"] == "main")
        or (s["name"] == "stdlib")
        or (s["name"] == "original" and args.include_original)
        or (s["name"] == "optimal" and args.include_optimal)
    ]

    # Initialize data dictionaries
    data_dict: Dict[str, Dict[str, float]] = {}
    for sol_type in SOLUTION_TYPES:
        name = sol_type["name"]
        data_dict[f"{name}_best"] = {}
        data_dict[f"{name}_all"] = {}

    problem_label: Dict[str, str] = {}

    stdlib_best_ratios: list[float] = []
    stdlib_all_ratios: list[float] = []

    # Process only hacker targets
    for target in hacker_targets:
        problem_label[target] = ""

        timeouted = {}
        not_found = False

        # Process each solution type
        for sol_type in SOLUTION_TYPES:
            name = sol_type["name"]
            try:
                time_to_best, elapsed_time = process_csv_file(
                    target, sol_type, args.cores, args.results_dir
                )
            except FileNotFoundError:
                not_found = True
                continue

            if elapsed_time is None and sol_type["name"] == "main":
                problem_label[target] += "*"

            timeouted[f"{name}_best"] = (
                time_to_best is None
            )
            timeouted[f"{name}_all"] = (
                elapsed_time is None
            )

            data_dict[f"{name}_best"][target] = (
                time_to_best if time_to_best is not None else 3600
            )
            data_dict[f"{name}_all"][target] = (
                elapsed_time if elapsed_time is not None else 3600
            )
        if not_found:
            continue

        if not timeouted["stdlib_best"] and not timeouted["main_best"]:
            stdlib_best_ratios.append(
                data_dict["stdlib_best"][target] / data_dict["main_best"][target]
            )
        if not timeouted["stdlib_all"] and not timeouted["main_all"]:
            stdlib_all_ratios.append(
                data_dict["stdlib_all"][target] / data_dict["main_all"][target]
            )

        # Add special markers for main solution
        if target in hacker_need_extra_targets:
            problem_label[target] += "★"

        # Mark initial cost improvement (using main solution data)
        filepath = os.path.join(args.results_dir, f"{target}.{args.cores}.csv")
        if check_cost_improvement(filepath):
            problem_label[target] += "♦"
    
    print("best acc ratio:", min(stdlib_best_ratios), max(stdlib_best_ratios))
    print("all acc ratio:", min(stdlib_all_ratios), max(stdlib_all_ratios))

    problems = list(data_dict["main_best"].keys())
    fig = plt.figure(figsize=(args.figure_width, args.figure_height))
    ax = fig.add_subplot(1, 1, 1)  # Create a single subplot

    # Calculate bar positions
    bar_width = 0.2
    positions = calculate_bar_positions(problems, SOLUTION_TYPES, bar_width)

    # Set x-axis labels
    xlabels = [f"{problem_label[p]}{p}" for p in problems]
    plt.xticks(
        positions["xaxis"],
        xlabels,
        rotation=40,
        ha="right",
        fontproperties=fm.FontProperties(stretch="ultra-condensed", size=args.font_size),
    )
    # Set up y-axis
    setup_y_axis(ax, fontsize=args.font_size)

    # Plot all bars
    plot_bars(ax, positions, data_dict, SOLUTION_TYPES, bar_width)

    # Add reference lines
    add_reference_lines(ax)

    # Save the plot
    plt.legend(loc="upper left", fontsize=args.font_size, framealpha=args.frame_alpha)
    save_plot(args.output_file)
