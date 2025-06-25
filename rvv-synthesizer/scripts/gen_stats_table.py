#!/usr/bin/env python3

import os
import csv
import math
import argparse
from targets import (
    hacker_targets,
    highway_targets,
    vqsort_success_targets,
    vqsort_failed_targets,
)

targets = (
    highway_targets
    + vqsort_success_targets
    + vqsort_failed_targets
    + hacker_targets
)

# Add argument parser
parser = argparse.ArgumentParser()
parser.add_argument(
    "--results-dir",
    type=str,
    help="Directory containing the results files",
    default=os.path.join(
        os.path.dirname(os.path.abspath(__file__)), "..", "results"
    ),
)
parser.add_argument(
    "-j",
    "--cores",
    type=int,
    help="Num of cores producing the data",
    default=72,
)
parser.add_argument(
    "--output-file",
    type=str,
    help="Output file name",
    default=os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "..",
        "analysis",
        "stats_table.tex",
    ),
)
args = parser.parse_args()

# Move parse_int_str outside the loop


def parse_int_str(s: str) -> int:
    if "e" in s:
        return int(float(s))
    elif "." in s:
        integer_part, decimal_part = s.split(".", 1)
        if decimal_part.replace("0", "") == "":
            return int(integer_part)
        else:
            return -1
    else:
        return int(s)


data = []
for target in targets:
    filename = f"{target}.{args.cores}.csv"
    file_path = os.path.join(args.results_dir, filename)
    if os.path.exists(file_path):
        with open(file_path, "r") as f:
            reader = csv.DictReader(f)
            for row in reader:  # Assuming only one row per csv
                # Use target directly instead of extracting from filename
                name = target  # This removes the .72 postfix

                # strip spaces from keys in row
                stripped_row = {}
                for key, value in row.items():
                    stripped_row[key.strip()] = value
                row = stripped_row

                # Truncate long names if needed
                # if "ZeroExtendResizeBitCast" in name:
                #     name = name.replace(
                #         "ZeroExtendResizeBitCast", "Zero...Cast"
                #     )

                comp = str(
                    parse_int_str(row.get("num_components", "-1"))
                    if target not in vqsort_failed_targets
                    else "?"
                )

                choice = str(
                    parse_int_str(row.get("avg_component_choices", "-1"))
                )

                ncr = str(
                    f"{math.comb(int(choice) + int(comp) - 1, int(comp)):.1e}".replace(
                        "+", ""
                    )
                    if comp != "?"
                    else "?"
                )

                data.append(
                    {
                        "Name": name.replace("_failure", "").replace(".Fixed", ".WithPrologue")
                        .replace("_Ascending", "")
                        .replace("_", "\\_"),
                        "#comp": comp,
                        "#choice": choice,
                        "#ncr": ncr,
                        "#prog": (
                            f"{parse_int_str(row.get('num_root_programs', '-1')):.1e}".replace(
                                "+", ""
                            )
                            if target not in vqsort_failed_targets
                            else "?"
                        ),
                        "#inst/Ref": str(
                            parse_int_str(row.get("num_ref_instructions", "-1"))
                        ),
                        "#inst/Best": (
                            str(
                                parse_int_str(
                                    row.get("min_num_instructions", "-1")
                                )
                                - (4 if target.endswith("Fixed") else 0)
                            )
                            if target not in vqsort_failed_targets
                            else "?"
                        ),
                        "Cost/Ref": str(
                            parse_int_str(row.get("initial_cost", "-1"))
                            - (1 if target in hacker_targets else 0)
                        ),
                        "Cost/Best": (
                            str(parse_int_str(row.get("cost", "-1")))
                            if target not in vqsort_failed_targets
                            else "?"
                        ),
                    }
                )

# data.sort(key=lambda x: x["Name"]) # Removed sorting

num_entries = len(data)
cols = 2
rows_per_col = math.ceil(num_entries / cols)

# Calculate maximum width for each column to align the raw LaTeX
col_widths = {
    "Name": max(len(entry["Name"]) for entry in data),
    "#comp": max(len(entry["#comp"]) for entry in data),
    "#ncr": max(len(entry["#ncr"]) for entry in data),
    "#prog": max(len(entry["#prog"]) for entry in data),
    "#inst/Ref": max(len(entry["#inst/Ref"]) for entry in data),
    "#inst/Best": max(len(entry["#inst/Best"]) for entry in data),
    "#choice": max(len(entry["#choice"]) for entry in data),
    "Cost/Ref": max(len(entry["Cost/Ref"]) for entry in data),
    "Cost/Best": max(len(entry["Cost/Best"]) for entry in data),
}

# Make column headers at least as wide as the column names
col_widths["Name"] = max(col_widths["Name"], 4)  # "Name"
col_widths["#comp"] = max(col_widths["#comp"], 3)  # "$k$"
col_widths["#ncr"] = max(col_widths["#ncr"], 4)  # "Leaf"
col_widths["#prog"] = max(col_widths["#prog"], 4)  # "prog"
col_widths["#inst/Ref"] = max(col_widths["#inst/Ref"], 7)  # "inst/Ref"
col_widths["#inst/Best"] = max(col_widths["#inst/Best"], 4)  # "inst/Best"
col_widths["#choice"] = max(col_widths["#choice"], 3)  # "$n$"
col_widths["Cost/Ref"] = max(col_widths["Cost/Ref"], 1)  # "Cost/Ref"
col_widths["Cost/Best"] = max(col_widths["Cost/Best"], 1)  # "Cost/Best"

latex_table = "\\documentclass{article}\n"
latex_table += "\\usepackage{booktabs}\n"
latex_table += "\\usepackage{multirow}\n"  # Add multirow package
latex_table += "\\begin{document}\n"
latex_table += "\\begin{table}[h]\n"
latex_table += "\\centering\n"
latex_table += "\\caption{Synthesis Statistics}\n"
latex_table += "\\begin{tabular}{ccccccccccc|ccccccccccc}\n"  # Added empty columns for spacing
latex_table += "\\hline\n"

# Create two-row header with categories
# First row (categories)
for j in range(cols):
    if j > 0:
        latex_table += " & "

    # For the 1st column add a multirow for "Name"
    latex_table += (
        f"\\multirow{{2}}{{*}}{{{'Name'.ljust(col_widths['Name'])}}} & "
    )

    # Space category spanning 3 columns (no need to align with data)
    latex_table += f"\\multicolumn{{4}}{{c}}{{Space}} & "

    # Empty column for spacing
    latex_table += f" & "

    # #inst category spanning 2 columns (no need to align with data)
    latex_table += f"\\multicolumn{{2}}{{c}}{{Inst}} & "

    # Empty column for spacing
    latex_table += f" & "

    if j == cols - 1:
        # Cost category spanning 2 columns (no need to align with data)
        latex_table += f"\\multicolumn{{2}}{{c}}{{Cost}}"
    else:
        latex_table += f"\\multicolumn{{2}}{{c|}}{{Cost}}"

latex_table += " \\\\\n"

# Replace cmidrules with hlines with breaks
for j in range(cols):
    offset = (
        j * 11
    )  # Increased from 8 to 10 columns per side due to spacing columns
    latex_table += f"\\cline{{{2+offset}-{5+offset}}} \\cline{{{7+offset}-{8+offset}}} \\cline{{{10+offset}-{11+offset}}}\n"

# Second row (sub-categories)
for j in range(cols):
    if j > 0:
        latex_table += " & "

    # Space sub-categories (aligned with data)
    latex_table += f"".ljust(col_widths["Name"]) + " & "
    latex_table += f"$k$".ljust(col_widths["#comp"]) + " & "
    latex_table += f"$n$".ljust(col_widths["#choice"]) + " & "
    latex_table += f"Leaf".ljust(col_widths["#ncr"]) + " & "
    latex_table += f"Prog".ljust(col_widths["#prog"]) + " & "

    # Empty column for spacing
    latex_table += "& "

    # #inst sub-categories (renamed)
    latex_table += f"R".ljust(col_widths["#inst/Ref"]) + " & "
    latex_table += f"B".ljust(col_widths["#inst/Best"]) + " & "

    # Empty column for spacing
    latex_table += "& "

    # Cost sub-categories
    latex_table += f"R".ljust(col_widths["Cost/Ref"]) + " & "
    latex_table += f"B".ljust(col_widths["Cost/Best"])

latex_table += " \\\\\n"
latex_table += "\\hline\n"

# Main body rows - unchanged
for i in range(rows_per_col):
    row_str = ""
    for j in range(cols):
        index = i + j * rows_per_col
        if j > 0:
            row_str += " & "
        if index == len(highway_targets):
            row_str += "\\cline{1-11}\n"

        if index < num_entries:
            entry = data[index]

            name_str = entry["Name"].ljust(col_widths["Name"])
            comp_str = entry["#comp"].ljust(col_widths["#comp"])
            choice_str = entry["#choice"].ljust(col_widths["#choice"])
            ncr_str = entry["#ncr"].ljust(col_widths["#ncr"])
            prog_str = entry["#prog"].ljust(col_widths["#prog"])
            instref_str = entry["#inst/Ref"].ljust(col_widths["#inst/Ref"])
            inst_str = entry["#inst/Best"].ljust(col_widths["#inst/Best"])
            ref_str = entry["Cost/Ref"].ljust(col_widths["Cost/Ref"])
            best_str = entry["Cost/Best"].ljust(col_widths["Cost/Best"])

            row_str += f"{name_str} & {comp_str} & {choice_str} & {ncr_str} & {prog_str} & & {instref_str} & {inst_str} & & {ref_str} & {best_str}"
        else:
            # Add empty cells to maintain alignment for missing entries
            row_str += "".ljust(col_widths["Name"]) + " & "
            row_str += "".ljust(col_widths["#comp"]) + " & "
            row_str += "".ljust(col_widths["#choice"]) + " & "
            row_str += "".ljust(col_widths["#ncr"]) + " & "
            row_str += "".ljust(col_widths["#prog"]) + " & "
            row_str += " & "  # Empty column for spacing
            row_str += "".ljust(col_widths["#inst/Ref"]) + " & "
            row_str += "".ljust(col_widths["#inst/Best"]) + " & "
            row_str += " & "  # Empty column for spacing
            row_str += "".ljust(col_widths["Cost/Ref"]) + " & "
            row_str += "".ljust(col_widths["Cost/Best"])
    row_str += " \\\\\n"
    latex_table += row_str

latex_table += "\\hline\n"
latex_table += "\\end{tabular}\n"
latex_table += "\\end{table}\n"
latex_table += "\\end{document}\n"

print(latex_table)


# Write to file
os.makedirs(os.path.dirname(args.output_file), exist_ok=True)
with open(args.output_file, "w") as f:
    f.write(latex_table)
