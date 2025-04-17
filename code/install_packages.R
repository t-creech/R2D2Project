# Install tidyverse package if not already installed
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
# Install rstan package if not already installed
if (!requireNamespace("rstan", quietly = TRUE)) {
  install.packages("rstan")
}
# Install ggplot2 package if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}