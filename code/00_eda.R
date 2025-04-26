# 00_eda.R  –  Exploratory Data Analysis for R2D2 Project
# Goal: This script reads the gambia dataset and produces a quick set of exploratory
# summaries and plots

# Workflow
#   1. Read the data
#   2. Create summary statistics for the numeric variables
#   3. Plot histograms of the numeric variables
#   6. Save the figures and summary statistics to the output directory

# Usage:
#   Rscript 00_eda.R <gambia_csv> <breast_tumors_csv> <out_dir>


# Arguments:
#   - gambia_csv: CSV file containing the data
#   - breast_tumors_csv: CSV file containing the breast tumors data
#   - out_dir: Directory to save the output figures and summary statistics

# Load required libraries
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  stop("Usage: Rscript eda.R <gambia_csv> <breast_tumors_csv> <out_dir>")
}

gambia_path <- args[1]
breast_tumors_path <- args[2]
out_dir <- args[3]
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Load the data
df_gambia <- read.csv(gambia_path, stringsAsFactors = FALSE)
df_breast_tumors <- read.csv(breast_tumors_path, stringsAsFactors = FALSE)

### Gambia data EDA
# Check for missing values
missing_values_gambia <- colSums(is.na(df_gambia))

# Check dimensions
gambia_dimensions <- dim(df_gambia)

# Check data types
gambia_data_types <- sapply(df_gambia, class)

# Check summary statistics
gambia_summary <- summary(df_gambia)

# Check correlations
gambia_nums <- vapply(df_gambia, is.numeric, logical(1))
gambia_correlations <- cor(df_gambia[ , gambia_nums], use = "pairwise.complete.obs")

### Breast tumors data EDA
# Check for missing values
missing_values_breast_tumors <- colSums(is.na(df_breast_tumors))

# Check dimensions
breast_tumors_dimensions <- dim(df_breast_tumors)

# Check data types
breast_tumors_data_types <- sapply(df_breast_tumors, class)

# Check summary statistics
breast_tumors_summary <- summary(df_breast_tumors)

# Check correlations for numeric variables
breast_tumors_numeric_vars <- sapply(df_breast_tumors, is.numeric)
breast_tumors_correlations <- cor(df_breast_tumors[, breast_tumors_numeric_vars], use = "pairwise.complete.obs")

# Open a text connection
txt_path <- file.path(out_dir, "gambia_eda.txt")
sink(txt_path)            # all subsequent console output goes to this file
# 1) Missing values
cat("=== Gambia: Missing Values ===\n")
print(missing_values_gambia)
# 2) Dimensions
cat("\n=== Gambia: Dimensions (rows, columns) ===\n")
print(gambia_dimensions)
# 3) Data types
cat("\n=== Gambia: Column Classes ===\n")
print(gambia_data_types)
# 4) Summary statistics
cat("\n=== Gambia: Summary() Output ===\n")
print(gambia_summary)
# 5) Correlations
cat("\n=== Gambia: Numeric Correlations ===\n")
print(gambia_correlations)
# Close the sink
sink()

# Open a text connection
txt_path <- file.path(out_dir, "breast_tumors_eda.txt")
sink(txt_path)            # all subsequent console output goes to this file
# 1) Missing values
cat("=== Breast Tumors: Missing Values ===\n")
print(missing_values_breast_tumors)
# 2) Dimensions
cat("\n=== Breast Tumors: Dimensions (rows, columns) ===\n")
print(breast_tumors_dimensions)
# 3) Data types
cat("\n=== Breast Tumors: Column Classes ===\n")
print(breast_tumors_data_types)
# 4) Summary statistics
cat("\n=== Breast Tumors: Summary() Output ===\n")
print(breast_tumors_summary)
# 5) Correlations
cat("\n=== Breast Tumors: Numeric Correlations ===\n")
print(breast_tumors_correlations)
# Close the sink
sink()