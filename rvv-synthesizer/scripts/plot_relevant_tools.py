#! /usr/bin/env python3

import matplotlib.pyplot as plt
from matplotlib.figure import Figure
from matplotlib.axes import Axes as MPLAxes
import numpy as np
import os
import argparse
import matplotlib.patches as mpatches
from matplotlib.ticker import ScalarFormatter
import csv
import sys
import math
from typing import Any, Dict, List, Tuple, Optional, Union, Sequence, cast
from matplotlib.figure import Figure
from matplotlib.path import Path
from matplotlib.markers import MarkerStyle
from adjustText import adjust_text
from plot_util import setup_plot_params

# Set plot parameters
setup_plot_params()

# Define custom types for better type checking
ToolData = Dict[str, Any]
DataPoint = Tuple[float, float]

# Add import for targets
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from targets import (
    highway_targets,
    hacker_targets,
    vqsort_success_targets,
    known_timeout_targets,
)

# Command line arguments
parser = argparse.ArgumentParser(
    description="Plot tradeoff curve for program synthesis tools"
)
parser.add_argument(
    "--output-file-prefix",
    type=str,
    help="Output file prefix",
    default=os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "..",
        "analysis",
        "figure_tradeoff",
    ),
)
parser.add_argument(
    "--include-highway",
    action="store_true",
    help="Include HieraSynth highway points in the plot",
    default=True,
)
parser.add_argument(
    "--exclude-highway",
    action="store_true",
    help="Exclude HieraSynth highway points from the plot",
)

parser.add_argument(
    "--include-hacker",
    action="store_true",
    help="Include HieraSynth hacker points in the plot",
    default=True,
)
parser.add_argument(
    "--exclude-hacker",
    action="store_true",
    help="Exclude HieraSynth hacker points from the plot",
)
parser.add_argument(
    "--include-vqsort",
    action="store_true",
    help="Include HieraSynth vqsort points in the plot",
    default=True,
)
parser.add_argument(
    "--exclude-vqsort",
    action="store_true",
    help="Exclude HieraSynth vqsort points from the plot",
)
parser.add_argument(
    "--results-dir",
    type=str,
    help="Directory containing the results files",
    default=os.path.join(
        os.path.dirname(os.path.abspath(__file__)), "..", "results"
    ),
)
args = parser.parse_args()

# Process argument combinations
if args.exclude_highway:
    args.include_highway = False
if args.exclude_hacker:
    args.include_hacker = False
if args.exclude_vqsort:
    args.include_vqsort = False

# Ensure the output directory exists
os.makedirs(os.path.dirname(args.output_file_prefix), exist_ok=True)


def create_tools_data() -> List[ToolData]:
    """
    Create a list of dictionaries containing data for various program synthesis tools.

    Returns:
        A list of tool data dictionaries.
    """
    # Data from the table as a list of dictionaries
    return [
        {
            "tool": "Brahma",
            "k": 16.0,
            "n": 20.0,
            "n_range": (
                20.0,
                20.0,
            ),  # Brahma: <20, but we're using 20 as a point
            "remark": "effective n is even smaller",
            "paper": "Brahma (PLDI'11)",
            "isa": "Bit-vector operations",
            "color": "#1f77b4",
            "marker": "o",
        },
        {
            "tool": "Massalin (ASPLOS II)",
            "k": 13.0,
            "n": 25.0,
            "n_range": (20.0, 30.0),  # Massalin
            "remark": "?",
            "paper": "Massalin (ASPLOS II)",
            "isa": "?",
            "color": "#aa3399",
            "marker": "o",
        },
        {
            "tool": "Brahma (RISC-V, optimal sketch)",
            "k": 12.0,
            "n": 31.0,
            "n_range": (31.0, 31.0),  # Brahma (RISC-V): 31
            "remark": "RISC-V variant",
            "paper": "Brahma (PLDI'11)",
            "isa": "RISC-V subset",
            "color": "#1f77b4",
            "marker": "o",
        },
        {
            "tool": "Lens (GA144)",
            "k": 12.0,
            "n": 23.0,
            "n_range": (23.0, 23.0),  # Lens (GA144): ~23 suggested by repo
            "remark": "suggested by their repo",
            "paper": "Lens (ASPLOS'16)",
            "isa": "Subset of GreenArrays GA144",
            "color": "#ff7f0e",
            "marker": "o",
        },
        {
            "tool": "Barthe et al. (PPoPP'13)",
            "k": 9.0,
            "n": 30.0,
            "n_range": (20.0, 40.0),  # PPoPP
            "remark": "?",
            "paper": "Barthe et al. (PPoPP'13)",
            "isa": "?",
            "color": "#888888",
            "marker": "o",
        },
        {
            "tool": "Souper",
            "k": 6.0,
            "n": 61.0,
            "n_range": (61.0, 61.0),  # Souper: 61 (51 op + 10 intrinsics)
            "remark": "51 op + 10 intrinsics",
            "paper": "Souper (arXiv 1711.04422)",
            "isa": "Custom ISA based on LLVM scalar ISA",
            "color": "#2ca02c",
            "marker": "o",
        },
        {
            "tool": "Lens (ARM)",
            "k": 5.0,
            "n": 61.0,
            "n_range": (61.0, 61.0),  # Lens (ARM): ~61 suggested by repo
            "remark": "suggested by their repo",
            "paper": "Lens (ASPLOS'16)",
            "isa": "Subset of ARM v7-A",
            "color": "#ff7f0e",
            "marker": "o",
        },
        {
            "tool": "Minotaur",
            "k": 1.0,
            "n": 250.0,
            "n_range": (200.0, 300.0),  # Minotaur: 250 fixed value
            "remark": "165 x86 intrinsics implemented",
            "paper": "Minotaur (OOPSLA'24)",
            "isa": "Subset of SSE/AVX/AVX2/AVX-512 + LLVM",
            "color": "#d62728",
            "marker": "o",
        },
        {
            "tool": "Bansal and Aiken (ASPLOS'06)",
            "k": 3,
            "n": 300,
            "n_range": (300.0, 300.0),
            "color": "#d62728",
            "marker": "o",
        },
        # HieraSynth will be added as multiple points from CSV files
    ]


def get_benchmark_category(target: str) -> str:
    """
    Determine the benchmark category for a given target.

    Args:
        target: The name of the target.

    Returns:
        The category of the benchmark: "highway", "hacker", "vqsort", or "unknown".
    """
    if target in highway_targets:
        return "highway"
    elif target in hacker_targets:
        return "hacker"
    elif target in vqsort_success_targets:
        return "vqsort"
    return "unknown"  # Add a default return value to fix type error


# Function to read HieraSynth data from CSV files
def read_hierasynth_data(use_selected_points: bool = False) -> List[ToolData]:
    """
    Read data from CSV files for HieraSynth benchmarks.

    Args:
        use_selected_points: Whether to filter for selected points (k=6,7,8 with max n)

    Returns:
        A list of dictionaries, each containing data for one benchmark.
    """
    # Define the directory where CSV files are located
    results_dir = args.results_dir

    # All targets to check
    all_targets: list[str] = []
    if args.include_highway:
        all_targets += highway_targets
    if args.include_hacker:
        all_targets += hacker_targets
    if args.include_vqsort:
        all_targets += vqsort_success_targets
    print(f"Total targets to check: {len(all_targets)}")

    # Initialize list to store data
    hierasynth_data: List[ToolData] = []

    # Color and marker for HieraSynth points
    hierasynth_color = "#e79aff"
    # Define markers for different categories
    category_markers = {
        "highway": "o",  # Circle for highway targets
        "hacker": "s",  # Square for hacker targets
        "vqsort": "D",  # Diamond for vqsort targets
        "unknown": "o",  # Default marker for unknown categories
    }

    # Process each target
    for target in all_targets:
        process_target(
            target,
            results_dir,
            hierasynth_data,
            hierasynth_color,
            category_markers,
        )

    print(f"Total valid data points: {len(hierasynth_data)}")

    # If no data was found, provide a default point
    if not hierasynth_data:
        print("Warning: No valid HieraSynth data found in CSV files")
        hierasynth_data = [
            {
                "tool": "Default",
                "k": 8.0,
                "n": 210.0,
                "category": "unknown",
                "color": hierasynth_color,
                "marker": "o",
                "is_timeout": False,
            }
        ]

    # If using selected points, filter for k=6,7,8 and keep only the max n for each k
    if use_selected_points:
        filtered_data = filter_selected_points(hierasynth_data)
        print(f"Filtered to {len(filtered_data)} selected points")
        return filtered_data

    return hierasynth_data


def process_target(
    target: str,
    results_dir: str,
    hierasynth_data: List[ToolData],
    hierasynth_color: str,
    category_markers: Dict[str, str],
) -> None:
    """
    Process a single target and add its data to hierasynth_data if valid.

    Args:
        target: The target name
        results_dir: Directory containing CSV files
        hierasynth_data: List to append valid data to
        hierasynth_color: Color to use for the data point
        category_markers: Mapping from category to marker style
    """
    # For 8-bit precision
    csv_path = os.path.join(results_dir, f"{target}.72.csv")
    print(csv_path)

    if os.path.exists(csv_path):
        try:
            # Read CSV file using built-in csv module
            with open(csv_path, "r") as f:
                reader = csv.DictReader(f, skipinitialspace=True)
                # Get the first row from the CSV
                for row in reader:
                    process_csv_row(
                        row,
                        target,
                        hierasynth_data,
                        hierasynth_color,
                        category_markers,
                    )
                    break  # Only process the first row
        except Exception as e:
            print(f"  Error reading {csv_path}: {e}")


def process_csv_row(
    row: Dict[str, str],
    target: str,
    hierasynth_data: List[ToolData],
    hierasynth_color: str,
    category_markers: Dict[str, str],
) -> None:
    """
    Process a CSV row and add the data to hierasynth_data if valid.

    Args:
        row: Dictionary containing CSV data
        target: The target name
        hierasynth_data: List to append valid data to
        hierasynth_color: Color to use for the data point
        category_markers: Mapping from category to marker style
    """
    # Check if required columns exist
    min_instr_key = "min_num_instructions"
    avg_choices_key = "avg_component_choices"

    if min_instr_key in row and avg_choices_key in row:
        try:
            k = float(row[min_instr_key])
            n = float(row[avg_choices_key])

            # Skip invalid values (inf, nan, negative, etc.)
            if math.isinf(k) or math.isnan(k) or k <= 0:
                print(f"  Skipping {target}: invalid k value ({k})")
                return

            if math.isinf(n) or math.isnan(n) or n <= 0:
                print(f"  Skipping {target}: invalid n value ({n})")
                return

            # Adjust k for specific targets
            if target in ["Min128.Fixed", "Lt128.Fixed"]:
                k -= 4

            # Determine category and marker
            category = get_benchmark_category(target)
            marker = category_markers.get(
                category, "o"
            )  # default marker is circle

            # Store as dictionary
            hierasynth_data.append(
                {
                    "tool": target,
                    "k": k,
                    "n": n,
                    "category": category,
                    "color": hierasynth_color,
                    "marker": marker,
                    "is_timeout": target in known_timeout_targets,
                }
            )

            print(f"  Target: {target}, k={k}, n={n}")
        except (ValueError, TypeError) as e:
            print(f"  Error converting values for {target}: {e}")
    else:
        print(f"  Missing required columns in CSV file for {target}")
        available_columns = list(row.keys())
        print(f"  Available columns: {available_columns}")


def filter_selected_points(hierasynth_data: List[ToolData]) -> List[ToolData]:
    """
    Filter HieraSynth data to only keep points with k=6,7,8 and maximum n values.

    Args:
        hierasynth_data: List of HieraSynth data dictionaries

    Returns:
        Filtered list with at most 3 data points
    """
    # Group data by k value
    points_by_k = {6: [], 7: [], 8: []}

    # Filter points with k=6,7,8
    for data in hierasynth_data:
        k = int(data["k"])
        if k in points_by_k:
            # Create a copy with unified marker and category
            point_copy = data.copy()
            point_copy["category"] = "HieraSynth (selected benchmarks)"
            point_copy["marker"] = "o"
            points_by_k[k].append(point_copy)

    # Find the point with maximum n for each k
    result = []
    for k in sorted(points_by_k.keys()):
        if points_by_k[k]:
            max_n_point = max(points_by_k[k], key=lambda x: float(x["n"]))
            result.append(max_n_point)

    return result


# Create a function to generate the plot
def create_plot(use_selected_points: bool = False) -> Figure:
    """
    Generate a log-scale plot comparing different program synthesis tools.

    Args:
        use_selected_points: Whether to create a plot with only selected points (k=6,7,8 with max n)

    Returns:
        The matplotlib figure object for the plot.
    """
    # Get tool data
    tools_data = create_tools_data()

    # Create figure and get axis object
    fig = plt.figure(figsize=(10, 8))
    ax = plt.gca()

    # Read HieraSynth data from CSV files
    hierasynth_data = read_hierasynth_data(use_selected_points)

    customize_plot_appearance(ax, tools_data, hierasynth_data)

    # Plot each type of data
    texts = plot_tool_data_points(ax, tools_data)
    all_data_points = collect_data_points(tools_data, hierasynth_data)
    # Create trendline
    plot_trendline(ax, tools_data, all_data_points)
    plot_hierasynth_points(ax, hierasynth_data, use_selected_points)

    # Add legend and customize plot appearance
    add_plot_legend(ax, use_selected_points)

    # Adjust text positions
    adjust_text_positions(ax, texts)

    # Adjust layout
    plt.tight_layout()

    return fig


def plot_tool_data_points(ax: MPLAxes, tools_data: List[ToolData]) -> List[Any]:
    """
    Plot data points for each tool in tools_data.

    Args:
        ax: The matplotlib axis to plot on
        tools_data: List of tool data dictionaries

    Returns:
        List of text objects for annotation adjustment
    """
    texts = []

    for tool_data in tools_data:
        # Slightly adjust alpha for Brahma (RISC-V) to distinguish it
        point_alpha = (
            0.8
            if tool_data["tool"]
            == "Brahma (RISC-V, our implementation, optimal sketch)"
            else 1.0
        )

        # Use different edge colors for Brahma variants
        edge_color = (
            "darkblue"
            if tool_data["tool"]
            == "Brahma (RISC-V, our implementation, optimal sketch)"
            else "black"
        )

        ax.scatter(
            tool_data["n"],
            tool_data["k"],
            s=150,
            color=tool_data["color"],
            marker=tool_data["marker"],
            zorder=4,
            edgecolors=edge_color,
            linewidths=1,
            alpha=point_alpha,
        )

        # Add tool annotations (to be adjusted later)
        text = ax.text(
            tool_data["n"],
            tool_data["k"],
            f"{tool_data['tool']}",
            fontsize=20,
            fontweight="bold",
            color=tool_data["color"],
            zorder=5,
            bbox=dict(
                boxstyle="round,pad=0.3",
                fc="white",
                alpha=0.85,
                ec=tool_data["color"],
                lw=0.5,
            ),
            ha="center",
            va="center",
        )
        texts.append(text)

        # Add horizontal error bars for n ranges if they exist
        n_range = tool_data["n_range"]
        if n_range[0] != n_range[1]:
            ax.hlines(
                y=tool_data["k"],
                xmin=n_range[0],
                xmax=n_range[1],
                colors=tool_data["color"],
                linestyles="-",
                alpha=0.4,
                linewidths=2,
            )

            # Add small vertical lines at the ends of the horizontal error bars
            ax.vlines(
                x=n_range[0],
                ymin=tool_data["k"] - 0.05 * tool_data["k"],
                ymax=tool_data["k"] + 0.05 * tool_data["k"],
                colors=tool_data["color"],
                linestyles="-",
                alpha=0.4,
            )
            ax.vlines(
                x=n_range[1],
                ymin=tool_data["k"] - 0.05 * tool_data["k"],
                ymax=tool_data["k"] + 0.05 * tool_data["k"],
                colors=tool_data["color"],
                linestyles="-",
                alpha=0.4,
            )

    return texts


def collect_data_points(
    tools_data: List[ToolData], hierasynth_data: List[ToolData]
) -> List[DataPoint]:
    """
    Collect all data points from both tools_data and hierasynth_data.

    Args:
        tools_data: List of tool data dictionaries
        hierasynth_data: List of HieraSynth data dictionaries

    Returns:
        List of (n, k) tuples for all data points
    """
    all_data_points = []

    # Add tools data points
    for tool_data in tools_data:
        all_data_points.append((float(tool_data["n"]), float(tool_data["k"])))

    # Add HieraSynth data points
    for data in hierasynth_data:
        all_data_points.append((float(data["n"]), float(data["k"])))

    return all_data_points


def plot_hierasynth_points(
    ax: MPLAxes,
    hierasynth_data: List[ToolData],
    use_selected_points: bool = False,
) -> None:
    """
    Plot data points for HieraSynth benchmarks.

    Args:
        ax: The matplotlib axis to plot on
        hierasynth_data: List of HieraSynth data dictionaries
        use_selected_points: Whether this is a plot with only selected points
    """
    # For legend labeling
    labelled = set()

    for data in hierasynth_data:
        category = data["category"]
        if category not in labelled:
            labelled.add(category)
            label = category
        else:
            label = None

        # Only use hatching if not in selected points mode
        hatch = (
            None
            if use_selected_points
            else ("////////" if data["is_timeout"] else None)
        )

        ax.scatter(
            data["n"],
            data["k"],
            s=150,
            color=data["color"],
            marker=data["marker"],
            zorder=4,
            edgecolors="black",
            linewidths=1,
            alpha=0.7,
            label=label,
            hatch=hatch,
        )


def plot_trendline(
    ax: MPLAxes, tools_data: List[ToolData], all_data_points: List[DataPoint]
) -> None:
    """
    Calculate and plot a power law trend line.

    Args:
        ax: The matplotlib axis to plot on
        tools_data: List of tool data dictionaries
        all_data_points: All data points as (n, k) tuples
    """
    # Create arrays for fitting the power law
    n_values_for_fit = [float(tool_data["n"]) for tool_data in tools_data]
    k_values_for_fit = [float(tool_data["k"]) for tool_data in tools_data]

    # Add a power law trend line (using log scale for fitting)
    z = np.polyfit(np.log(n_values_for_fit), np.log(k_values_for_fit), 1)
    p = np.poly1d(z)

    # Calculate R-squared for the fit
    y_log_actual = np.log(k_values_for_fit)
    y_log_pred = p(np.log(n_values_for_fit))
    ss_total = np.sum((y_log_actual - np.mean(y_log_actual)) ** 2)
    ss_residual = np.sum((y_log_actual - y_log_pred) ** 2)
    r_squared = 1 - (ss_residual / ss_total)

    # Generate x values for trend line
    min_n = min([float(data["n_range"][0]) for data in tools_data])
    max_n = max([float(data["n_range"][1]) for data in tools_data])
    x_trend = np.linspace(min_n * 0.8, max_n * 1.2, 100)
    y_trend = np.exp(p(np.log(x_trend)))

    print(
        f"fitted: log(k) = {z[0]:.2f} * log(n) + {z[1]:.2f}, R² = {r_squared:.3f}"
    )

    # Plot the trend line
    ax.plot(
        x_trend,
        y_trend,
        "r-",
        alpha=0.7,
        zorder=1,
        linewidth=2,
        label=f"log(k) = {z[0]:.2f} * log(n) + {z[1]:.2f}, R² = {r_squared:.3f}",
    )

    # Print various analysis metrics
    print_analysis_metrics(all_data_points, z)


def print_analysis_metrics(
    all_data_points: List[DataPoint], z: np.ndarray
) -> None:
    """
    Print various analysis metrics based on the data points and fitted curve.

    Args:
        all_data_points: List of (n, k) data points
        z: Parameters of the fitted power law
    """
    print(f"points: {all_data_points}")

    # Calculate various metrics based on the power law fit
    slope, intercept = z[0], z[1]

    print(
        f"points (log(k) - {slope:.2f} * log(n)): {[np.log(k) - slope * np.log(n) for n, k in all_data_points]}"
    )
    print(
        f"np.exp((log(k) - {intercept:.2f}) / {slope:.2f}): {[np.exp((np.log(k) - intercept) / slope) for n, k in all_data_points]}"
    )
    print(
        f"n - np.exp((log(k) - {intercept:.2f}) / {slope:.2f}): {[n - np.exp((np.log(k) - intercept) / slope) for n, k in all_data_points]}"
    )
    print(
        f"k-expected = exp(({intercept:.2f} + {slope:.2f} * log(n))): {[np.exp((intercept + slope * np.log(n))) for n, k in all_data_points]}"
    )
    print(
        f"k-advantage = k - k-expected: {[k - np.exp((intercept + slope * np.log(n))) for n, k in all_data_points]}"
    )
    print(
        f"log(k)-advantage: {[np.log(k) - (intercept + slope * np.log(n)) for n, k in all_data_points]}"
    )
    print(
        f"max log(k)-advantage: {max([np.log(k) - (intercept + slope * np.log(n)) for n, k in all_data_points])}"
    )


def add_plot_legend(ax: MPLAxes, use_selected_points: bool = False) -> None:
    """
    Add a legend to the plot.

    Args:
        ax: The matplotlib axis to add the legend to
        use_selected_points: Whether this is a plot with only selected points
    """
    # Get existing handles and labels
    handles, _ = ax.get_legend_handles_labels()

    # Add the "Cannot prove optimality" patch only if not in selected points mode
    if not use_selected_points:
        hatched_patch = mpatches.Patch(
            color="black",
            label="Cannot prove optimality",
            hatch="/////",
            fill=False,
        )
        # Combine existing handles with our custom patch
        handles.append(hatched_patch)

    ax.legend(handles=handles, loc="lower left", fontsize=23, framealpha=0.5)


def customize_plot_appearance(
    ax: MPLAxes,
    tools_data: List[ToolData],
    hierasynth_data: List[ToolData],
) -> None:
    """
    Customize the appearance of the plot.

    Args:
        ax: The matplotlib axis to customize
        tools_data: List of tool data dictionaries
        hierasynth_data: List of HieraSynth data dictionaries
    """
    # Set axis scales to logarithmic
    plt.xscale("log")
    plt.yscale("log")
    # Format the axes to show non-scientific notation
    for axis in [ax.xaxis, ax.yaxis]:
        axis.set_major_formatter(ScalarFormatter())

    # Set labels and title with better styling
    plt.xlabel("n (Size of Instruction Set)", fontsize=26)
    plt.ylabel("k (Maximum Program Size)", fontsize=26)
    plt.xticks(fontsize=24)
    plt.yticks(fontsize=24)

    # Set axis limits
    set_axis_limits(ax, tools_data, hierasynth_data)

    # Add grid for better readability
    plt.grid(True, alpha=0.3, linestyle="--", which="both")


def set_axis_limits(
    ax: MPLAxes,
    tools_data: List[ToolData],
    hierasynth_data: List[ToolData],
) -> None:
    """
    Set appropriate axis limits based on the data.

    Args:
        ax: The matplotlib axis to set limits for
        tools_data: List of tool data dictionaries
        hierasynth_data: List of HieraSynth data dictionaries
    """
    # Ensure we have valid data points
    valid_tool_n_values = [
        float(data["n"])
        for data in tools_data
        if float(data["n"]) > 0
        and not math.isinf(float(data["n"]))
        and not math.isnan(float(data["n"]))
    ]
    valid_tool_k_values = [
        float(data["k"])
        for data in tools_data
        if float(data["k"]) > 0
        and not math.isinf(float(data["k"]))
        and not math.isnan(float(data["k"]))
    ]

    valid_hierasynth_n = [
        float(data["n"])
        for data in hierasynth_data
        if float(data["n"]) > 0
        and not math.isinf(float(data["n"]))
        and not math.isnan(float(data["n"]))
    ]
    valid_hierasynth_k = [
        float(data["k"])
        for data in hierasynth_data
        if float(data["k"]) > 0
        and not math.isinf(float(data["k"]))
        and not math.isnan(float(data["k"]))
    ]

    max_x = (
        max(
            max(valid_tool_n_values),
            max(valid_hierasynth_n) if valid_hierasynth_n else 0,
        )
        * 1.5
    )
    max_y = (
        max(
            max(valid_tool_k_values),
            max(valid_hierasynth_k) if valid_hierasynth_k else 0,
        )
        * 1.5
    )
    plt.xlim(15, max_x)
    plt.ylim(0.8, max_y)


def adjust_text_positions(ax: MPLAxes, texts: List[Any]) -> None:
    """
    Adjust the positions of text annotations to avoid overlaps.

    Args:
        ax: The matplotlib axis containing the texts
        texts: List of text objects to adjust
    """
    if texts:
        # Configure how the text should be adjusted
        adjust_text(
            texts,
            arrowprops=dict(
                arrowstyle="->",
                color="black",
                lw=1.5,
                alpha=0.7,
                shrinkA=10,
                shrinkB=3,
            ),
            expand=(1.2, 1.2),
            force_text=(20.0, 20.0),  # Strength of repulsion between texts
            force_explode=(1.7, 1.7),
            force_static=(1.0, 1.0),
            pull_threshold=0.0,
            explode_radius=500,
            ensure_inside_axes=True,
            avoid_self=True,
            iter_lim=1000,  # Maximum number of iterations for best results
            time_lim=10,
            # autoalign="xy",  # Auto-align texts
            min_arrow_len=0,
            prevent_crossings=True,
            max_move=20,
        )


def main():
    """Main entry point for the script."""
    # Create plot with all points
    fig_all = create_plot(use_selected_points=False)
    output_file_all = f"{args.output_file_prefix}_curve.pdf"
    fig_all.savefig(output_file_all, dpi=300, bbox_inches="tight")
    print(f"Figure with all points saved to {output_file_all}")

    # Also save a PNG version
    png_output_all = output_file_all.replace(".pdf", ".png")
    fig_all.savefig(png_output_all, dpi=300, bbox_inches="tight")
    print(f"PNG version with all points saved to {png_output_all}")
    plt.close()

    # Create plot with only selected points
    fig_selected = create_plot(use_selected_points=True)
    output_file_selected = f"{args.output_file_prefix}_curve_selected.pdf"
    fig_selected.savefig(output_file_selected, dpi=300, bbox_inches="tight")
    print(f"Figure with selected points saved to {output_file_selected}")

    # Also save a PNG version
    png_output_selected = output_file_selected.replace(".pdf", ".png")
    fig_selected.savefig(png_output_selected, dpi=300, bbox_inches="tight")
    print(f"PNG version with selected points saved to {png_output_selected}")
    plt.close()


if __name__ == "__main__":
    main()
