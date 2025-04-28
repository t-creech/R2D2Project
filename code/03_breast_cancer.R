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

library(tidyverse)
library(Matrix)
library(brms)

# Read and preprocess data
data <- read.csv(breast_cancer)
data <- data %>% mutate(Y = ifelse(Sample.treatment == "BE", 1, 0))
X <- as.matrix(data[, 4:(ncol(data)-1)])
y <- data$Y
for (j in 1:ncol(X)) {
  X[is.na(X[,j]), j] <- mean(X[,j], na.rm = TRUE)
}

# Performance metrics
brier_score <- function(y_true, y_pred) mean((y_true - y_pred)^2)
binary_cross_entropy <- function(y_true, y_pred) {
  # y_pred must be probabilities in (0,1)
  eps <- 1e-15
  y_pred <- pmin(pmax(y_pred, eps), 1 - eps)
  -mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
}

# Efficient, minimal MCMC Sampler for Logistic Regression
run_r2d2_logistic_mcmc <- function(X, y, gbp_params, n_iter = 2000, burn_in = 200) {
  n <- nrow(X)
  p <- ncol(X)
  beta <- matrix(0, nrow = n_iter, ncol = p)
  beta0 <- numeric(n_iter)
  # Robustly extract tau2 from gbp_params (list or numeric)
  if (is.list(gbp_params) && !is.null(gbp_params$tau2)) {
    tau2 <- gbp_params$tau2
  } else if (is.numeric(gbp_params)) {
    tau2 <- gbp_params
  } else {
    tau2 <- 1
  }
  beta_curr <- rep(0, p)
  beta0_curr <- 0
  for (iter in 1:n_iter) {
    beta_prop <- beta_curr + rnorm(p, 0, 0.05)
    beta0_prop <- beta0_curr + rnorm(1, 0, 0.05)
    eta_curr <- as.numeric(X %*% beta_curr + beta0_curr)
    eta_prop <- as.numeric(X %*% beta_prop + beta0_prop)
    loglike_curr <- sum(y * eta_curr - log1p(exp(eta_curr)))
    loglike_prop <- sum(y * eta_prop - log1p(exp(eta_prop)))
    logprior_curr <- sum(dnorm(beta_curr, 0, sqrt(tau2), log=TRUE)) + dnorm(beta0_curr, 0, 10, log=TRUE)
    logprior_prop <- sum(dnorm(beta_prop, 0, sqrt(tau2), log=TRUE)) + dnorm(beta0_prop, 0, 10, log=TRUE)
    log_alpha <- (loglike_prop + logprior_prop) - (loglike_curr + logprior_curr)
    if (log(runif(1)) < log_alpha) {
      beta_curr <- beta_prop
      beta0_curr <- beta0_prop
    }
    beta[iter, ] <- beta_curr
    beta0[iter] <- beta0_curr
  }
  # Discard burn-in
  beta <- beta[(burn_in+1):n_iter, , drop=FALSE]
  beta0 <- beta0[(burn_in+1):n_iter]
  return(list(beta = beta, beta0 = beta0))
}

# --- Efficient settings for development/testing ---
n_repeats <- 50  # Decrease to 5 for testing 
brms_iter <- 2000  # Decrease to 1000 for testing
brms_chains <- 2   # Use 1 for test runs
mcmc_iter <- 11000  # Decrease to 2000 for testing
mcmc_burn <- 1000   # Decrease to 200 for testing

results <- list()

# ---- HORSESHOE PRIOR (using brms) ----
bs_vec <- numeric(n_repeats)
bce_vec <- numeric(n_repeats)
for (i in 1:n_repeats) {
  idx <- sample(seq_len(nrow(X)), size = 0.75 * nrow(X))
  X_train <- X[idx, ]
  y_train <- y[idx]
  X_test <- X[-idx, ]
  y_test <- y[-idx]
  mu <- colMeans(X_train)
  sd <- apply(X_train, 2, sd)
  X_train_std <- scale(X_train, center = mu, scale = sd)
  X_test_std <- scale(X_test, center = mu, scale = sd)
  train_df <- data.frame(Y = y_train, X_train_std)
  fit <- brm(
    formula = Y ~ .,
    data = train_df,
    family = bernoulli(),
    prior = prior(horseshoe(), class = "b"),
    chains = brms_chains, iter = brms_iter, refresh = 0, silent = 2
  )
  beta_post_mean <- as.numeric(fixef(fit)[-1, "Estimate"])
  beta0_post_mean <- fixef(fit)[1, "Estimate"]
  # Compute probabilities (plogis) for BCE!
  y_pred <- plogis(as.numeric(X_test_std %*% beta_post_mean + beta0_post_mean))
  bs_vec[i] <- brier_score(y_test, y_pred)
  bce_vec[i] <- binary_cross_entropy(y_test, y_pred)
}
results[["Horseshoe"]] <- list(
  BS = mean(bs_vec), BS_se = sd(bs_vec)/sqrt(n_repeats),
  BCE = mean(bce_vec), BCE_se = sd(bce_vec)/sqrt(n_repeats)
)

# ---- R2D2 PRIORS (GBP approximation, custom MCMC) ----
r2d2_priors <- list(
  "Beta(1,5)" = list(a = 1, b = 5),
  "Beta(1,10)" = list(a = 1, b = 10),
  "Beta(1,20)" = list(a = 1, b = 20),
  "Beta(1,30)" = list(a = 1, b = 30)
)

for (prior_name in names(r2d2_priors)) {
  prior <- r2d2_priors[[prior_name]]
  bs_vec <- numeric(n_repeats)
  bce_vec <- numeric(n_repeats)
  for (i in 1:n_repeats) {
    idx <- sample(seq_len(nrow(X)), size = 0.75 * nrow(X))
    X_train <- X[idx, ]
    y_train <- y[idx]
    X_test <- X[-idx, ]
    y_test <- y[-idx]
    mu <- colMeans(X_train)
    sd <- apply(X_train, 2, sd)
    X_train_std <- scale(X_train, center = mu, scale = sd)
    X_test_std <- scale(X_test, center = mu, scale = sd)
    beta0_init <- qlogis(mean(y_train))
    gbp_params <- match_gbp(prior$a, prior$b, beta0 = beta0_init, family = "binomial")
    mcmc <- run_r2d2_logistic_mcmc(
      X = X_train_std, y = y_train,
      gbp_params = gbp_params,
      n_iter = mcmc_iter, burn_in = mcmc_burn
    )
    beta_post_mean <- colMeans(mcmc$beta)
    beta0_post_mean <- mean(mcmc$beta0)
    y_pred <- plogis(as.numeric(X_test_std %*% beta_post_mean + beta0_post_mean))
    bs_vec[i] <- brier_score(y_test, y_pred)
    bce_vec[i] <- binary_cross_entropy(y_test, y_pred)
  }
  results[[prior_name]] <- list(
    BS = mean(bs_vec), BS_se = sd(bs_vec)/sqrt(n_repeats),
    BCE = mean(bce_vec), BCE_se = sd(bce_vec)/sqrt(n_repeats)
  )
}

# ---- OUTPUT RESULTS ----
cat("Method\tBS (SE)\tBCE (SE)\n")
for (m in names(results)) {
  r <- results[[m]]
  cat(sprintf("%s\t%.2f (%.2f)\t%.2f (%.2f)\n", m, r$BS, r$BS_se, r$BCE, r$BCE_se))
}

# Save results
saveRDS(results, file = file.path(out_dir, paste0("breast_cancer_results_", seed, ".rds")))
cat("Saved results to", file.path(out_dir, paste0("breast_cancer_results_", seed, ".rds")), "\n")
