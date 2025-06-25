#!/usr/bin/env python3

import csv
import sys


def add(l, r):
    m = dict()
    for k in l:
        m[k] = float(l[k]) + float(r[k])
    return m


def div(l, n):
    m = dict()
    for k in l:
        m[k] = float(l[k]) / n
    return m


def weighted_average_percentages(rows):
    """Calculate weighted average for percentage fields using unique_viable_nodes as weights"""
    percentage_fields = ["regular_success_percentage", "generalization_success_percentage"]
    
    result = {}
    
    for field in percentage_fields:
        if field not in rows[0]:
            continue
            
        total_weighted_sum = 0.0
        total_weights = 0.0
        
        for row in rows:
            if "unique_viable_nodes" in row and field in row:
                weight = float(row["unique_viable_nodes"])
                percentage = float(row[field])
                total_weighted_sum += weight * percentage
                total_weights += weight
        
        if total_weights > 0:
            result[field] = total_weighted_sum / total_weights
        else:
            result[field] = 0.0
    
    return result


fieldnames = ["num_root_programs",
              "num_components",
              "avg_component_choices",
              "scheduler_elapsed_time",
              "time_to_best_since_scheduler_start",
              "time_to_best",
              "min_num_instructions",
              "num_ref_instructions",
              "initial_cost",
              "cost",
              "num_lattice_nodes",
              "num_undertermined_leaves",
              "num_undertermined_programs",
              "undetermined_ratio",
              "unique_viable_nodes",
              "unique_generalization_failure_nodes",
              "regular_success_percentage",
              "generalization_success_percentage",
              ]

if __name__ == '__main__':
    files = sys.argv[1:]
    rows = []
    for file in files:
        with open(file) as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                rows.append(row)
                # print(row)
    # print(len(rows))
    
    # Calculate simple average for most fields
    base = rows[0]
    for r in rows[1:]:
        base = add(base, r)
    result = div(base, len(rows))
    
    # Calculate weighted average for percentage fields
    percentage_averages = weighted_average_percentages(rows)
    
    # Override the percentage fields with weighted averages
    for field, value in percentage_averages.items():
        result[field] = value
    
    first = True
    for field in fieldnames:
        if field not in result:
            continue
        if not first:
            print(", ", end="")
        print(field, end="")
        first = False
    print()
    first = True
    for field in fieldnames:
        if field not in result:
            continue
        if not first:
            print(", ", end="")
        print(result[field], end="")
        first = False
    print()
