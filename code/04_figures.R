# 04_figures.R  –  Figure Contruction for R2D2 Project
# Goal: This script aims to recreate the figures presented in the paper

# Workflow
#   1. Read the data
#   2. Perform analysis
#   3. Plot figures and create tables
#   6. Save the figures and tables to the output directory

# Usage:
#   Rscript 04_figures.R <gambia> <breast_tumors> <out_dir>


# Arguments:
#   - gambia: Data file containing the data
#   - breast_tumors: Data file containing the breast tumors data
#   - out_dir: Directory to save the output figures and summary statistics

# Load required libraries
library(ggplot2)
library(patchwork)

# Pull in the functions from the previous scripts
source("code/01_prior_construction.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  stop("Usage: Rscript eda.R <gambia> <breast_tumors> <out_dir>")
}
gambia <- args[1]
breast_tumors <- args[2]
out_dir <- args[3]
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## Figure 1: Prior distributions of W to induce R2 ~ Beta(a, b) with B_0 = 0
a <- c(1, 4)
b <- c(1, 4)
family <- c("gaussian", "poisson")
# Create a data frame for parameter combinations
priors <- expand.grid(
    a      = a,
    b      = b,
    family = family,
    stringsAsFactors = FALSE
)

# Create a data frame for the prior distributions
prior_df <- data.frame()
for (i in 1:nrow(priors)) {
    a_i <- priors$a[i]
    b_i <- priors$b[i]
    family_i <- priors$family[i]
  
    # Generate the prior distribution
    # Define the range of W values
    wgrid <- seq(0, 5, length.out = 100)

    # Calculate the prior density
    prior_density <- exact_r2d2_prior(w = wgrid, a = a_i, b = b_i, beta0 = 0, family = family_i)
  
    # Create a data frame for the current parameter combination
    temp_df <- data.frame(
        W = wgrid,
        Density = prior_density,
        a = a_i,
        b = b_i,
        family = family_i
    )
  
    # Append to the main data frame
    prior_df <- rbind(prior_df, temp_df)
}
# Create the plot of densities for different families and parameters
gg <- ggplot(prior_df, aes(x = W, y = Density, colour = interaction(a, b))) +
  geom_line(size = 1) +
  facet_wrap(
    ~ family,
    nrow = 1,
    labeller = labeller(family = c(gaussian = "Normal", poisson = "Poisson"))
  ) +
  scale_colour_brewer(
    palette = "Set2",
    name   = "(a, b)",
    labels = c("1,1", "1,4", "4,1", "4,4")
  ) +
  labs(
    x = "W",
    y = "Prior Density"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.background    = element_blank(),
    strip.text          = element_text(face = "bold", size = 16),
    legend.position     = "bottom",
    legend.title.align  = 0.5
  )

# Save to file
ggsave(
  filename = file.path(out_dir, "Figure1_priors.png"),
  plot     = gg,
  width    = 10,
  height   = 4
)

## Figure 2: Comparison of the priors for
# Create a data frams for each of the needed prior distributions
priors <- data.frame(
    a      = c(1, 1, 4),
    b      = c(1, 4, 1),
    family = c("poisson"),
    stringsAsFactors = FALSE
)
# Create a data frame for the prior distributions calculated by each method
prior_exact <- data.frame()
prior_linear <- data.frame()
prior_gbp <- data.frame()

# Loop through the parameter combinations and calculate the prior densities
for (i in 1:nrow(priors)) {
    a_i <- priors$a[i]
    b_i <- priors$b[i]
    family_i <- priors$family[i]
  
    # Generate the prior distribution
    # Define the range of W values
    wgrid <- seq(0.01, 3, length.out = 100)

    # Calculate the prior density using the exact method
    prior_density_exact <- exact_r2d2_prior(w = wgrid, a = a_i, b = b_i, beta0 = 0, family = family_i)
  
    # Create a data frame for the current parameter combination
    temp_df_exact <- data.frame(
        W = wgrid,
        Density = prior_density_exact,
        Method = "Exact",
        a = a_i,
        b = b_i,
        family = family_i
    )
  
    # Append to the main data frame
    prior_exact <- rbind(prior_exact, temp_df_exact)

    # Calculate the prior density using the linear approximation method
    # Generate the parameter values
    params <- linear_gbp_params(a_i, b_i, beta0 = 0, family = family_i)
    prior_density_linear <- dgbp(wgrid, params[1], params[2], params[3], params[4])
    # Create a data frame for the current parameter combination
    temp_df_linear <- data.frame(
        W = wgrid,
        Density = prior_density_linear,
        Method = "Linear Approximation",
        a = a_i,
        b = b_i,
        family = family_i
    )
    # Append to the main data frame
    prior_linear <- rbind(prior_linear, temp_df_linear)

    # Calculate the prior density using the generalized beta prime method
    params <- match_gbp(a_i, b_i, beta0 = 0, lambda = 0.25, family = family_i)
    prior_density_gbp <- dgbp(wgrid, params[1], params[2], params[3], params[4])
    # Create a data frame for the current parameter combination
    temp_df_gbp <- data.frame(
        W = wgrid,
        Density = prior_density_gbp,
        Method = "Generalized Beta Prime",
        a = a_i,
        b = b_i,
        family = family_i
    )
    # Append to the main data frame
    prior_gbp <- rbind(prior_gbp, temp_df_gbp)
}
# Combine the data frames for all methods
prior_df <- rbind(prior_exact, prior_linear, prior_gbp)
prior_df$combo <- paste0("a=", prior_df$a, ", b=", prior_df$b)
# Create the plot of densities for different families and parameters
gg <- ggplot(prior_df, aes(x = W, y = Density, colour = Method, linetype = Method)) +
  geom_line(size = 1) +
  facet_wrap(~ combo, nrow = 1, scales = "free_y") +
  scale_colour_brewer(
    palette = "Set2",
    name    = "Method"
  ) +
  scale_linetype_manual(
    name   = "Method",
    values = c(
      "Exact"                   = "solid",
      "Generalized Beta Prime"  = "dashed",
      "Linear Approximation"    = "solid"
    )
  ) +
  labs(
    x = "W",
    y = "Prior Density"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 16),
    legend.position    = "bottom",
    legend.title.align = 0.5
  )

ggsave(
  filename = file.path(out_dir, "Figure2_PriorsMethods.png"),
  plot     = gg,
  width    = 10,
  height   = 4
)

## Figure 3: Prior R^2 and global variance parameters for R2D2 prior for Gambia data
beta0 <- -0.59
family <- "binomial"
# Create a data frame for the prior distributions
priors <- data.frame(
    a      = c(0.5, 1, 1, 4, 4),
    b      = c(0.5, 1, 4, 1, 4),
    family = family,
    stringsAsFactors = FALSE
)

# Calculate the prior R2 and W prior distributions
r2_prior_df <- data.frame()
w_prior_df <- data.frame()

r2_grid <- seq(0.01, 0.99, length.out = 100)
w_grid <- seq(0.025, 4, length.out = 200)

for (i in 1:nrow(priors)) {
    a_i <- priors$a[i]
    b_i <- priors$b[i]
    family_i <- priors$family[i]
  
    # Calculate R2 density
    r2_density <- dbeta(r2_grid, a_i, b_i)
    # Calculate W density
    w_params <- match_gbp(a_i, b_i, beta0 = beta0, lambda = 0.25, family = family_i)
    w_density <- dgbp(w_grid, w_params[1], w_params[2], w_params[3], w_params[4])

    # Store the R2 and W density in a data frame
    temp_r2_df <- data.frame(
        R2 = r2_grid,
        Density = r2_density,
        a = a_i,
        b = b_i,
        family = family_i
    )
    temp_w_df <- data.frame(
        W = w_grid,
        Density = w_density,
        a = a_i,
        b = b_i,
        family = family_i
    )
    # Append to the main data frame
    r2_prior_df <- rbind(r2_prior_df, temp_r2_df)
    w_prior_df <- rbind(w_prior_df, temp_w_df)
}

# Plot vs W
gg_w <- ggplot(w_prior_df, aes(x = W, y = Density, colour = interaction(a, b))) +
  geom_line(size = 1) +
  facet_wrap(~ family, nrow = 1, labeller = labeller(family = c(binomial = "Logistic"))) +
  scale_colour_brewer(palette = "Set2", name = "(a, b)") +
  labs(x = "W", y = "Prior Density") +
  theme_bw(base_size = 14) +
  theme(
    strip.background  = element_blank(),
    strip.text        = element_text(face = "bold", size = 16),
    legend.position   = "bottom",
    legend.title.align= 0.5
  )

# Plot vs R2
gg_r2 <- ggplot(r2_prior_df, aes(x = R2, y = Density, colour = interaction(a, b))) +
  geom_line(size = 1) +
  facet_wrap(~ family, nrow = 1, labeller = labeller(family = c(binomial = "Logistic"))) +
  scale_colour_brewer(palette = "Set2", name = "(a, b)") +
  labs(x = expression(R^2), y = "Prior Density") +
  theme_bw(base_size = 14) +
  theme(
    strip.background  = element_blank(),
    strip.text        = element_text(face = "bold", size = 16),
    legend.position   = "bottom",
    legend.title.align= 0.5
  )

combined <- gg_r2 + gg_w + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")

ggsave(
  filename = file.path(out_dir, "Figure3_side_by_side.png"),
  plot     = combined,
  width    = 12,
  height   = 4
)

## Table 1: Generalized Beta Prime Approximations for Gambia data
# Create a data frame for the prior distributions
gbp_df <- data.frame(
    a      = c(1, 0.5, 1, 4, 4),
    b      = c(4, 0.5, 1, 4, 1),
    stringsAsFactors = FALSE
)

# Calculate the parameters for the generalized beta prime distribution
gbp_params <- data.frame()
for (i in 1:nrow(gbp_df)) {
    a_i <- gbp_df$a[i]
    b_i <- gbp_df$b[i]
  
    # Calculate the parameters for the generalized beta prime distribution
    params <- match_gbp(a_i, b_i, beta0 = beta0, lambda = 0.25, family = family)
  
    # Create a data frame for the current parameter combination
    temp_df <- data.frame(
        a = a_i,
        b = b_i,
        a_star = params[1],
        b_star = params[2],
        c_star = params[3],
        d_star = params[4]
    )
  
    # Append to the main data frame
    gbp_params <- rbind(gbp_params, temp_df)
}

# Save the table to a CSV file
write.csv(
    gbp_params,
    file = file.path(out_dir, "Table1_gbp_params.csv"),
    row.names = FALSE
)