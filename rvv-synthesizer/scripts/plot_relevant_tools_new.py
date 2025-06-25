#! /usr/bin/env python3

from matplotlib.ticker import ScalarFormatter
from matplotlib.axes import Axes as MPLAxes
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from scipy.stats import t
import numpy as np
from matplotlib.figure import Figure
import csv
import math
import os
import sys
import argparse
from typing import Any, Dict, List, Tuple
from plot_util import setup_plot_params

# Define custom types for better type checking
ToolData = Dict[str, Any]
DataPoint = Tuple[float, float]
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from targets import (
    highway_targets,
    hacker_targets,
    vqsort_success_targets,
    known_timeout_targets,
)


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
    all_tools_data = [
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
        {
            "tool": "Aquarium",
            "k": 5,
            "n": 57,
            "n_range": (34.0, 57.0),
            "color": "#d62728",
            "marker": "o",
        }
        # HieraSynth will be added as multiple points from CSV files
    ]
    return sorted(all_tools_data, key=lambda x: (x["n"], x["k"]))


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
    # print(csv_path)

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

            # print(f"  Target: {target}, k={k}, n={n}")
        except (ValueError, TypeError) as e:
            print(f"  Error converting values for {target}: {e}")
    else:
        print(f"  Missing required columns in CSV file for {target}")
        available_columns = list(row.keys())
        print(f"  Available columns: {available_columns}")


def filter_selected_points(hierasynth_data: List[ToolData]) -> List[ToolData]:
    """
    Filter HieraSynth data to only keep points with k=1,4,6,7,8.
    Returns top-3 points for k=6 and top-1 for k=1,4,7,8 based on maximum n values.

    Args:
        hierasynth_data: List of HieraSynth data dictionaries

    Returns:
        Filtered list with at most 6 data points (3 for k=6, 1 for k=1,4,7,8)
    """
    # Group data by k value
    points_by_k = {4: [], 6: [], 7: [], 8: []}
    num_points_by_k = {4: 1, 6: 4, 7: 2, 8: 1}

    # Filter points with k=6,7,8
    for data in hierasynth_data:
        k = int(data["k"])
        if k in points_by_k:
            # Create a copy with unified marker and category
            point_copy = data.copy()
            point_copy["category"] = "HieraSynth (selected benchmarks)"
            point_copy["marker"] = "o"
            points_by_k[k].append(point_copy)

    # Select top points for each k value
    result = []
    for k in sorted(points_by_k.keys()):
        if points_by_k[k]:
            # Sort points by n value in descending order
            sorted_points = sorted(points_by_k[k], key=lambda x: float(x["n"]), reverse=True)
            
            result.extend(sorted_points[:num_points_by_k[k]])

    return result


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

def n_to_u(ns):
    return 1 / np.log(ns)


def customize_plot_appearance(
    ax: MPLAxes
) -> None:
    """
    Customize the appearance of the plot.

    Args:
        ax: The matplotlib axis to customize
        tools_data: List of tool data dictionaries
        hierasynth_data: List of HieraSynth data dictionaries
    """
    # Format the axes to show non-scientific notation
    for axis in [ax.xaxis, ax.yaxis]:
        axis.set_major_formatter(ScalarFormatter())

    # Set labels and title with better styling
    plt.xticks(fontsize=24)
    plt.yticks(fontsize=24)

    # Add grid for better readability
    plt.grid(True, alpha=0.3, linestyle="--", which="both")

    # Draw a baseline to emphasize k = 0
    ax.axhline(y=0, color='black', linewidth=1.2)
    # Make sure the y-axis range includes 0 so the baseline is visible
    ymin, ymax = ax.get_ylim()
    if ymin > 0:
        ax.set_ylim(bottom=0)


def create_plot(use_selected_points: bool = False) -> Figure:
    tools_data = create_tools_data()
    tools_name = [tool["tool"] for tool in tools_data]
    y_fit = np.array([tool["k"] for tool in tools_data])
    x_fit = np.array([tool["n"] for tool in tools_data])
    tool_labels = [chr(x + ord('A')) for x in range(len(tools_data))]
    for name, label in zip(tools_name, tool_labels):
        print(f"{name} ({label})")
    u_fit = n_to_u(x_fit)
    print(u_fit)

    # Read HieraSynth data (highway/hacker/vqsort) - NOT used for fitting
    hierasynth_data = read_hierasynth_data(use_selected_points=use_selected_points)

    # ------------- Linear regression y = k*u + b ------------
    # Only use existing tools data for fitting, NOT HieraSynth data
    X = np.column_stack([u_fit, np.ones_like(u_fit)])
    k_hat, b_hat = np.linalg.lstsq(X, y_fit, rcond=None)[0]

    # Metrics
    y_pred_fit = X @ np.array([k_hat, b_hat])
    R2 = 1 - np.sum((y_fit - y_pred_fit) ** 2) / np.sum((y_fit - y_fit.mean()) ** 2)

    fitted_line_rep = f"k = {k_hat:.2f}/log(n) + {b_hat:.2f}"
    print("Fitted line: ", fitted_line_rep)
    print("R2: ", R2)

    # Residual variance and XtX inverse
    n, p = X.shape
    resid = y_fit - y_pred_fit
    s2 = resid @ resid / (n - p)
    XtX_inv = np.linalg.inv(X.T @ X)

    # Extend u_line range to include HieraSynth data
    all_u_values = list(u_fit)
    if hierasynth_data:
        hierasynth_u = [n_to_u(data["n"]) for data in hierasynth_data]
        all_u_values.extend(hierasynth_u)

    u_min = min(all_u_values) * 0.95
    u_max = max(all_u_values) * 1.05
    
    # Smooth curve for plotting
    u_line = np.linspace(u_min, u_max, 500)
    y_line = k_hat * u_line + b_hat

    # Standard error of mean prediction
    se_line = np.sqrt([
        np.array([ui, 1.0]) @ XtX_inv @ np.array([ui, 1.0]) * s2
        for ui in u_line
    ])

    # t-critical values for CIs
    t95  = t.ppf(0.975, df=n - p)
    t997 = t.ppf(1 - 0.003/2, df=n - p)

    y_lower95, y_upper95 = y_line - t95 * se_line, y_line + t95 * se_line
    y_lower997, y_upper997 = y_line - t997 * se_line, y_line + t997 * se_line

    if use_selected_points:
        fig, ax = plt.subplots(figsize=(12, 7.5), constrained_layout=True)
    else:
        fig, ax = plt.subplots(figsize=(9, 7), constrained_layout=True)

    # Confidence bands
    ax.fill_between(u_line, y_lower997, y_upper997, color='gold', alpha=0.25,
                    label='99.7% confidence band')
    ax.fill_between(u_line, y_lower95, y_upper95, color='coral', alpha=0.35,
                    label='95% confidence band')

    # Fitted line & existing tools data
    ax.plot(u_line, y_line, color='tab:orange', linewidth=2, label="{} (R^2={:.2f})".format(fitted_line_rep, R2))
    if use_selected_points:
        ax.scatter(u_fit, y_fit, marker='x', color='tab:red', label='Existing tools (best performance)')
        for yi, ui, label in zip(y_fit, u_fit, tool_labels):
            ax.annotate(label, (ui, yi), fontsize=20, ha='left', va='center')

    # Plot HieraSynth data points
    plot_hierasynth_points(ax, hierasynth_data)

    ax.set_xlabel('n (Size of Instruction Set)', fontsize=26)
    ax.set_ylabel(r'k (Maximum Program Size)', fontsize=26)
    # ax.set_title('Fit of $y = k / log(x) + b$ with confidence bands')

    def x_to_u(x): return 1/np.log(x)

    customize_plot_appearance(ax)

    # Set main axis ticks to show n values at appropriate u positions (after customize_plot_appearance)
    n_values = [20, 50, 100, 200, 400, 800]
    ax.set_xticks([x_to_u(n) for n in n_values])
    ax.set_xticklabels([str(n) for n in n_values])

    # u_min, u_max = ax.get_xlim()
    k_min, k_max = ax.get_ylim()
    
    # ---------------- Additional analysis for k=7 and k=8 points (only for selected plot) ----------------
    if use_selected_points:
        for k_target in (6, 7, 8):
            k_points = [d for d in hierasynth_data if int(round(d["k"])) == k_target]
            if not k_points:
                continue

            rightmost_pt = max(k_points, key=lambda d: d["n"])
            n_val = rightmost_pt["n"]
            k_val = rightmost_pt["k"]
            u_val = n_to_u(n_val)

            # Draw horizontal and vertical dashed lines through the point
            # ax.axvline(x=u_val, color="gray", linestyle="--", linewidth=1)
            ax.plot([u_val, u_val], [k_min, k_val], color="gray", linestyle="--", linewidth=1)
            # ax.axhline(y=k_val, color="gray", linestyle="--", linewidth=1, xmax=u_val)
            ax.plot([u_max, u_val], [k_val, k_val], color="gray", linestyle="--", linewidth=1)

            # Predicted k at this n
            k_pred = k_hat * u_val + b_hat

            # Predicted n for this k (invert regression)
            try:
                n_pred = math.exp(k_hat / (k_val - b_hat)) if k_val != b_hat else float("inf")
            except OverflowError:
                n_pred = float("inf")

            # Print results
            print(f"Rightmost k={k_target} point analysis:")
            print(f"  n = {n_val:.2f}, k = {k_val:.2f}")
            print(f"  k - k_hat = {k_val - k_pred:.2f}")
            print(f"  n - n_hat = {n_val - n_pred:.2f}")
            print(f"  k / k_hat = {k_val / k_pred:.2f}")
            if n_pred != 0 and not math.isinf(n_pred):
                n_ratio = n_val / n_pred
                print(f"  n / n_hat = {n_ratio:.2f}")

            # Annotate ratios on the figure when greater than 1 (actual exceeds prediction)
            k_ratio = k_val / k_pred if k_pred != 0 else float('inf')
            n_ratio = n_val / n_pred if n_pred not in (0, float('inf')) else None

            annotation_position = {6: (0, -15), 7: (0, 35), 8: (0, 63)}

            annotation_lines = []
            if k_ratio and k_ratio > 1.0:
                annotation_lines.append(f"k = {k_ratio:.2f}x")
            if n_ratio and n_ratio > 1.0:
                annotation_lines.append(f"n = {n_ratio:.2f}x")

            if annotation_lines:
                ax.annotate("\n".join(annotation_lines),
                            (u_val, k_val),
                            xytext=annotation_position[k_target],
                            textcoords='offset points',
                            fontsize=20,
                            ha='center', va='top',
                            bbox=dict(boxstyle='round,pad=0.2', fc='white', alpha=0.7))
    # ----------------------------------------------------------------------------------------------

    ax.set_xlim(u_min, u_max)
    ax.set_ylim(k_min, k_max)

    # Invert x-axis so that n goes from 20 to 800 (left to right)
    ax.invert_xaxis()

    # Add legend with timeout hatching explanation
    add_plot_legend(use_selected_points, ax)

    return fig


def plot_hierasynth_points(ax: MPLAxes, hierasynth_data: List[ToolData]) -> None:
    """
    Plot HieraSynth data points with proper markers and hatching for timeouts.

    Args:
        ax: The matplotlib axis to plot on
        hierasynth_data: List of HieraSynth data dictionaries
    """
    # For legend labeling
    labelled = set()

    for data in hierasynth_data:
        category = data["category"]
        u_val = n_to_u(data["n"])
        
        # Create label for legend
        if category not in labelled:
            labelled.add(category)
            label = category
        else:
            label = None

        # Add hatching for cases where we can prove optimality
        hatch = "////////" if not data["is_timeout"] else None

        ax.scatter(
            u_val,
            data["k"],
            s=150,
            color=data["color"],
            marker=data["marker"],
            zorder=4,
            edgecolors="black",
            linewidths=1,
            alpha=0.7,
            hatch=hatch,
        )

        # proxy for legend, no hatching
        ax.scatter(
            [], [], s=150,
            color=data["color"],
            marker=data["marker"],
            zorder=4,
            edgecolors="black",
            linewidths=1,
            alpha=0.7,
            label=label
        )




def add_plot_legend(use_selected_points: bool, ax: MPLAxes) -> None:
    """
    Add a legend to the plot including timeout hatching explanation.

    Args:
        ax: The matplotlib axis to add the legend to
    """
    # Get existing handles and labels
    handles, labels = ax.get_legend_handles_labels()

    # Add the "Can prove optimality" patch for non-timeout hatching
    hatched_patch = mpatches.Patch(
        color="black",
        label="Optimality proven",
        hatch="/////",
        fill=False,
    )
    # Combine existing handles with our custom patch
    handles.append(hatched_patch)

    if use_selected_points:
        ax.legend(handles=handles, loc='lower left', fontsize=22)
    else:
        ax.legend(handles=handles, loc='upper left', fontsize=22, framealpha=0.5)


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


# Set plot parameters
setup_plot_params()


if __name__ == "__main__":
    main()
else:
    # For backward compatibility when imported
    create_plot()
