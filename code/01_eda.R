# 01_eda.R  –  Exploratory Data Analysis for R2D2 Project
# Goal: This script reads the gambia dataset and produces a quick set of exploratory
# summaries and plots

# Workflow
#   1. Read the data
#   2. Create summary statistics for the numeric variables
#   3. Plot histograms of the numeric variables
#   4. Plot a pairs plot of the numeric variables
#   6. Save the figures and summary statistics to the output directory

# Usage:
#   Rscript 02_eda.R <gambia_csv> <out_dir>


# Arguments:
#   - gambia_csv: CSV file containing the data
#   - out_dir: Directory to save the output figures and summary statistics

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript 01_eda.R <gambia_csv> <out_dir>")
}

csv_path <- args[1]
out_dir <- args[2]
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Load the data
df <- read.csv(csv_path, stringsAsFactors = FALSE)