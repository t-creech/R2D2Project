# Load data from csv file
gambia <- read.csv("gambia_data.csv")
# Load the package
library("r2d2glmm")

# Look at data
head(gambia)

# Perform the analysis
summary(gambia)