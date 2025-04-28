# Install tidyverse package if not already installed
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "https://cloud.r-project.org")
}

# Install ggplot2 package if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
}

# Install stats package if not already installed
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats", repos = "https://cloud.r-project.org")
}

# Install patchwork package if not already installed
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork", repos = "https://cloud.r-project.org")
}

# Install Horseshoe if not already installed
if (!requireNamespace("horseshoe", quietly = TRUE)) 
    install.packages("horseshoe", repos = "https://cloud.r-project.org")

# Install Matrix package if not already installed
if (!requireNamespace("Matrix", quietly = TRUE)) 
    install.packages("Matrix", repos = "https://cloud.r-project.org")

# Install brms package if not already installed
if (!requireNamespace("brms", quietly = TRUE)) 
    install.packages("brms", repos = "https://cloud.r-project.org")

# Install dplyr package if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr", repos = "https://cloud.r-project.org")
}

# Install MASS package if not already installed
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS", repos = "https://cloud.r-project.org")
}

# Install mvtnorm package if not already installed
if (!requireNamespace("mvtnorm", quietly = TRUE)) {
  install.packages("mvtnorm", repos = "https://cloud.r-project.org")
}

# Install BayesLogit package if not already installed
if (!requireNamespace("BayesLogit", quietly = TRUE)) {
  install.packages("BayesLogit", repos = "https://cloud.r-project.org")
}

# Installing this package requires many requirements to be met including having JAGS installed
# Install rjags package if not already installed
if (!requireNamespace("rjags", quietly = TRUE)) {
  install.packages("rjags", repos = "https://cloud.r-project.org")
}

# Install coda package if not already installed
if (!requireNamespace("coda", quietly = TRUE)) {
  install.packages("coda", repos = "https://cloud.r-project.org")
}