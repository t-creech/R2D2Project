# 03_breast_cancer.R  –  Performs MH sampling to estimate the posteriors from Breast Cancer data
# Goal: This script reads the breast cancer dataset and recreates the MCMC sampling from the paper

# Workflow
#   1. Read the data
#   2. Transform the data as needed
#   3. Perform MCMC sampling to estimate the posteriors
#   4. Save the results to the output directory

# Usage:
#   Rscript 03_breast_cancer.R <breast_cancer_data> <seed> <out_dir>


# Arguments:
#   - breast_cancer_data: Path to the breast cancer data file
#   - seed: Random seed for reproducibility
#   - out_dir: Directory to save sampling results

# Pull in the functions from the previous scripts
source("code/01_prior_construction.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  stop("Usage: Rscript 03_breast_cancer.R <breast_cancer_data> <seed> <out_dir>")
}
breast_cancer <- args[1]
seed <- as.numeric(args[2])
out_dir <- args[3]
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Set the seed for reproducibility
set.seed(seed)

# Read the breast cancer data
breast_cancer_data <- read.csv(breast_cancer, header = TRUE)

# Transform the data as needed