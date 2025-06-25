#!/usr/bin/env python3

import matplotlib.pyplot as plt
from matplotlib import font_manager as fm
import os
import numpy as np
import argparse
from plot_util import setup_plot_params, setup_y_axis, save_plot

parser = argparse.ArgumentParser()

parser.add_argument(
    "--output-dir",
    type=str,
    help="Output directory for plots",
    default=os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "..",
        "analysis",
    ),
)

# Data points
mul_odd_data = [1, 7, 34, 97, 107, 77, 64, 48, 67, 70, 34]
mul_odd_label = "MulOdd"

add_sub_data = [1, 5, 19, 44, 73, 64, 46, 34, 59, 33, 22, 11]
add_sub_label = "AddSub"

sort_pairs_distance1_128_data = [
    1,
    9,
    44,
    103,
    237,
    566,
    2156,
    4776,
    7051,
    12784,
    6344,
]
sort_pairs_distance1_128_label = "SortPairsDistance1_128"
sort_pairs_distance1_128_cutoff_depth = 10

min128_data = [
    1,
    9,
    44,
    153,
    215,
    429,
    1073,
    2327,
    3689,
    6393,
    12479,
    3858,
    389,
    403,
    458,
    340,
    88,
]
min128_label = "Min128"
min128_cutoff_depth = 11


def plot_split_line(
    ax, x, y, cutoff, style, label, color, linewidth=2, markersize=8
):
    """Plot a line that changes from solid to dotted at a cutoff point."""
    # Plot solid line up to cutoff
    ax.plot(
        x[:cutoff],
        y[:cutoff],
        style,
        label=label,
        color=color,
        linewidth=linewidth,
        markersize=markersize,
    )
    # Plot dotted line after cutoff
    if cutoff < len(x):
        # Replace the line style dash with dots
        dotted_style = style.replace("-", ":")
        ax.plot(
            x[cutoff - 1 :],  # Overlap one point for continuity
            y[cutoff - 1 :],
            dotted_style,
            color=color,
            linewidth=linewidth,
            markersize=markersize,
        )


def generate_plot(use_log_scale: bool) -> plt.Figure:
    """Generate a single plot with either linear or log scale."""
    # Set plot parameters
    setup_plot_params()
    # Create figure and axis
    fig = plt.figure(figsize=(9, 6))
    ax = fig.add_subplot(1, 1, 1)
    plt.xticks(fontsize=25)
    plt.yticks(fontsize=25)

    ax.set_xlabel("Node depth", fontsize=25)
    ax.set_ylabel("Number of ever-started nodes", fontsize=25)

    # Create x-axis points
    x_mul_odd = np.arange(len(mul_odd_data))
    x_add_sub = np.arange(len(add_sub_data))
    x_sort = np.arange(len(sort_pairs_distance1_128_data))
    x_min = np.arange(len(min128_data))

    # Plot lines with different colors and markers
    # MulOdd and AddSub don't have cutoffs, so plot them normally
    ax.plot(
        x_mul_odd,
        mul_odd_data,
        "o-",
        label=mul_odd_label,
        color="green",
        linewidth=2,
        markersize=8,
    )
    ax.plot(
        x_add_sub,
        add_sub_data,
        "s-",
        label=add_sub_label,
        color="blue",
        linewidth=2,
        markersize=8,
    )

    # Plot split lines for those with cutoffs
    plot_split_line(
        ax,
        x_sort,
        sort_pairs_distance1_128_data,
        sort_pairs_distance1_128_cutoff_depth,
        "^-",
        sort_pairs_distance1_128_label,
        "orange",
    )
    plot_split_line(
        ax,
        x_min,
        min128_data,
        min128_cutoff_depth,
        "D-",
        min128_label,
        "darkviolet",
    )

    # Set y-axis scale
    if use_log_scale:
        ax.set_yscale("log")
        # Set y-axis limits to avoid log(0) issues
        ax.set_ylim(bottom=0.1)

    # Add legend
    ax.legend(
        loc="upper left",
        fontsize=25,
        #frameon=True,
        #fancybox=True,
        framealpha=0.6,
        #edgecolor="black",
    )

    # Add grid
    ax.grid(True, linestyle="--", alpha=0.7)

    # Set labels
    return fig


if __name__ == "__main__":
    args = parser.parse_args()

    # Generate linear scale plot
    fig_linear = generate_plot(use_log_scale=False)
    save_plot(
        os.path.join(args.output_dir, "figure_ever_started_trend_linear.pdf")
    )
    plt.close(fig_linear)

    # Generate log scale plot
    fig_log = generate_plot(use_log_scale=True)
    save_plot(
        os.path.join(args.output_dir, "figure_ever_started_trend_log.pdf")
    )
    plt.close(fig_log)
