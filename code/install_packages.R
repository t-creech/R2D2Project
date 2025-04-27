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
