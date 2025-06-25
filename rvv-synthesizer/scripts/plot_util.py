#!/usr/bin/env python3

from typing import Dict, List, Tuple, Any, Optional
import matplotlib.pyplot as plt
from matplotlib import font_manager as fm
import os
import csv


def time_to_float(s: str, cap: Optional[float] = None) -> Optional[float]:
    """Convert a time string to a float, capping at 3600.

    Args:
        s: Time string to convert

    Returns:
        Float value of the time, between 0 and 3600
    """
    if "inf" in s:
        return None
    if cap is None:
        return float(s)
    else:
        r = float(s)
        if r > cap:
            return None
        if r < 0:
            raise ValueError("Time is negative")
        return r


def add_reference_lines(ax: Any) -> None:
    """Add horizontal reference lines to the plot.

    Args:
        ax: Matplotlib axis to add reference lines to
    """
    for y_value in [10, 100, 500, 1000, 3600]:
        ax.axhline(y=y_value, color="grey", linestyle="--", lw=1)


def setup_plot_params():
    """Set up common plot parameters."""
    # plt.rcParams["xtick.labelsize"] = 20
    # plt.rcParams["ytick.labelsize"] = 20
    # plt.rcParams["legend.fontsize"] = 20
    fm.fontManager.addfont("scripts/LinBiolinum_Rah.ttf")
    fm.fontManager.addfont("scripts/LinBiolinum_Kah.ttf")
    fm.fontManager.addfont("scripts/LinBiolinum_RBah.ttf")
    fm.fontManager.addfont("scripts/LinBiolinum_RIah.ttf")
    plt.rcParams["font.family"] = "Linux Biolinum"


def setup_y_axis(ax: Any, fontsize: int = 25):
    """Set up the y-axis with logarithmic scale and common ticks.

    Args:
        ax: Matplotlib axis to configure
    """
    ax.set_yscale("log")
    plt.yticks(
        [1, 10, 100, 500, 1000, 3600],
        ["1", "10", "100", "500", "1000", "3600"],
        fontsize=fontsize,
    )


def process_csv_row(
    clean_row: Dict[str, str],
    target: str,
    known_timeout_targets: List[str],
    cap: Optional[float] = None,
) -> Tuple[Optional[float], Optional[float]]:
    """Process a single CSV row to extract timing information.

    Args:
        clean_row: Dictionary with cleaned column names
        target: The target name
        known_timeout_targets: List of targets known to time out

    Returns:
        Tuple of (time_to_best, scheduler_elapsed_time)
    """
    # Special handling for known timeout targets
    if target in known_timeout_targets:
        return (
            time_to_float(clean_row["time_to_best_since_scheduler_start"], cap),
            None,
        )

    # For all targets
    if "cost" in clean_row and float(clean_row.get("cost", -1)) < 0:
        return None, None

    return (
        time_to_float(clean_row["time_to_best_since_scheduler_start"], cap),
        time_to_float(clean_row["scheduler_elapsed_time"], cap),
    )


def save_plot(output_file: str):
    """Save the current plot to a file.

    Args:
        output_file: Path to save the plot
    """
    plt.tight_layout()
    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    plt.savefig(output_file)


def check_cost_improvement(filepath: str) -> bool:
    """Check if there's a significant cost improvement in the results.

    Args:
        filepath: Path to the CSV file to check

    Returns:
        True if there's a significant improvement, False otherwise
    """
    if not os.path.isfile(filepath):
        return False

    with open(filepath, "r") as f:
        reader = csv.DictReader(f)
        for row in reader:
            clean_row = {k.strip(): v for k, v in row.items()}
            if (
                float(clean_row["initial_cost"]) - float(clean_row["cost"])
                > 1.9
            ):
                return True
    return False
