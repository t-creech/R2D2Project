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

# Install horseshoe, tidyverse and Matrix package if not already installed
if (!requireNamespace("horseshoe", quietly = TRUE)) 
    install.packages("horseshoe", repos = "https://cloud.r-project.org")
if (!requireNamespace("tidyverse", quietly = TRUE)) 
    install.packages("tidyverse", repos = "https://cloud.r-project.org")
if (!requireNamespace("Matrix", quietly = TRUE)) 
    install.packages("Matrix", repos = "https://cloud.r-project.org")
if (!requireNamespace("brms", quietly = TRUE)) 
    install.packages("brms", repos = "https://cloud.r-project.org")

