# 06_fig_supp.R  –  Figure Contruction for Supplemental R2D2 Project
# Goal: This script aims to recreate the figures presented in the paper

# Workflow
#   1. Read the data
#   2. Perform analysis
#   3. Plot figures and create tables
#   4. Save the figures and tables to the output directory

# Usage:
#   Rscript 06_figs_supp.R <gambia> <breast_tumors> <seed> <out_dir>


# Arguments:
#   - gambia: Directory of RDS files containing the data
#   - breast_tumors: Data file containing the breast tumors data
#   - seed: Random seed for reproducibility
#   - out_dir: Directory to save the output figures and summary statistics

# Load required libraries
library(ggplot2)
library(patchwork)
library(tidyverse)

# Pull in the functions from the previous scripts
source("code/01_prior_construction.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  stop("Usage: Rscript 04_figures.R <gambia> <breast_tumors> <seed> <out_dir>")
}
gambia <- args[1]
breast_tumors <- args[2]
seed <- as.numeric(args[3])
out_dir <- args[4]
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## Figure 4: Posterior distributions of W for Gambia data
# Create a data frame for the posterior distributions
gambia_df <- data.frame()
# Determine the number of gambia runs
gambia_runs <- length(list.files(gambia, pattern = "gambia_"))
seed <- 1234
as <- c(0.5, 1, 1, 4, 4)
bs <- c(0.5, 1, 4, 1, 4)

for (i in 1:gambia_runs){
    # Load the MCMC samples
    mcmc_samples <- readRDS(file.path(gambia, paste0("gambia_", as[i], "_", bs[i], "_", seed, ".rds")))
  
    # Extract the posterior samples for W
    w_samples <- mcmc_samples$W_draws
    rho_samples <- mcmc_samples$rho_draws
    alpha_samples <- mcmc_samples$alpha_draws
    dd2_samples <- mcmc_samples$dd2_draws
    a <- as[i]
    b <- bs[i]
  
    # Create a data frame for the posterior samples
    temp_df <- data.frame(
        W = w_samples,
        rho = rho_samples,
        phi2 = dd2_samples,
        a = a,
        b = b
    )
  
    # Append to the main data frame
    gambia_df <- rbind(gambia_df, temp_df)
}

# Create a new column for R^2 and RE variance
beta0_init <- -0.59
gambia_df <- gambia_df %>%
  mutate(
    R2 = sapply(W, function(w) 
      W_to_R2(w, beta0 = beta0_init, family = "binomial")
    ),
    RE_Var = W * phi2
  )

# Create a new columns for the prior name
gambia_df$prior <- paste0("R2D2:a=", gambia_df$a, ",b=", gambia_df$b)

# Clean up the data frame
df_long <- gambia_df %>%
  pivot_longer(
    cols = c(W, RE_Var, R2, rho),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(
    stat = recode(stat,
      W = "W",
      RE_Var = "RE Var",
      R2 = "R2",
      rho = "rho"
    ),
    prior = factor(prior, levels = c(
      "R2D2:a=0.5,b=0.5",
      "R2D2:a=1,b=1",
      "R2D2:a=1,b=4",
      "R2D2:a=4,b=1",
      "R2D2:a=4,b=4"
    ))
  )

df <- df_long

# Plot the posterior distributions
p_W <- ggplot(filter(df, stat=="W"), aes(value, colour=prior))+
  geom_density()+
  scale_x_continuous(
    limits = c(0,15),
    breaks = seq(0,15, by=5)
  )+
  labs(x="W")+
  theme_bw()

p_RE <- ggplot(filter(df, stat=="RE Var"), aes(value, colour=prior))+
  geom_density()+
  scale_x_continuous(
    limits = c(0,15),
    breaks = seq(0,15, by=5)
  )+
  labs(x="RE Var")+
  theme_bw()

p_R2 <- ggplot(filter(df, stat=="R2"), aes(value, colour=prior))+
  geom_density()+
  scale_x_continuous(
    limits = c(0.0,0.62),
    breaks = seq(0.0,0.6, by=0.15)
  )+
  labs(x=expression(R^2))+
  theme_bw()

p_rho <- ggplot(filter(df, stat=="rho"), aes(value, colour=prior))+
  geom_density()+
  scale_x_continuous(
    limits = c(0,30000),
    breaks = seq(0,30000, by=10000)
  )+
  labs(x=expression(rho))+
  theme_bw()

# then stitch them side by side
combined <- (p_R2 | p_W) / (p_rho | p_RE) + plot_layout(guides="collect")
ggsave(filename = file.path(out_dir, "Figure4_posterior.png"), combined, width=10, height=6)

# Export the posterior samples to a CSV file
write.csv(
    gambia_df,
    file = file.path(out_dir, "posterior_samples_gambia.csv"),
    row.names = FALSE
)

## Table 2: Summary statistics for the posterior samples
# Create a summary table for the posterior samples
summary_wide_df <- gambia_df %>%
  group_by(prior) %>%
  summarise(
    W_mean = mean(W, na.rm = TRUE),
    W_sd = sd(W, na.rm = TRUE),
    RE_Var_mean = mean(RE_Var, na.rm = TRUE),
    RE_Var_sd = sd(RE_Var, na.rm = TRUE),
    R2_mean = mean(R2, na.rm = TRUE),
    R2_sd = sd(R2, na.rm = TRUE),
    rho_mean = mean(rho, na.rm = TRUE),
    rho_sd = sd(rho, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Prior = prior)

# Save the summary table to a CSV file
write.csv(
  summary_wide_df,
  file = file.path(out_dir, "Table2_Gambia_summary_statistics.csv"),
  row.names = FALSE
)