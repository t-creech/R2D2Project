# 02_gambia.R  –  Performs MH sampling to estimate the posteriors from Gambia data
# Goal: This script reads the gambia dataset and recreates the MCMC sampling from the paper

# Workflow
#   1. Read the data
#   2. Transform the data as needed
#   3. Perform MCMC sampling to estimate the posteriors
#   4. Save the results to the output directory

# Usage:
#   Rscript 02_gambia.R <gambia_data> <seed> <out_dir>


# Arguments:
#   - gambia_data: Path to the Gambia data file
#   - seed: Random seed for reproducibility
#   - out_dir: Directory to save sampling results

# Pull in the functions from the previous scripts
source("code/01_prior_construction.R")

# Load required libraries
library(numDeriv)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  stop("Usage: Rscript 02_gambia.R <gambia_data> <seed> <out_dir>")
}
gambia <- args[1]
seed <- as.numeric(args[2])
out_dir <- args[3]
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Set the seed for reproducibility
set.seed(seed)

# Define functions needed in the script
# Logit link log-likelihood
logit_ll <- function(y, eta){sum( y*eta - log1p(exp(eta)))}

# Log of prior density functions based on the paper
log_prior_beta <- function(beta, W, phi1, p) {-0.5 * p * log(2*pi*W*phi1/p) - 0.5 * (p/(W*phi1)) * sum(beta^2)}
log_prior_beta0 <- function(beta0, mu0, tau2) {-0.5 * ((beta0 - mu0)^2) / tau2 - 0.5 * log(2 * pi * tau2)}
log_prior_u <- function(u, W, phi2, C) {
    L <- length(u)
    Q <- solve(C)
    quad <- t(u) %*% Q %*% u
    -0.5 * (L*log(2*pi*W*phi2) + log(det(C)) + quad/(W*phi2))
}
log_prior_phi <- function(phi, xi = c(1,1)){
  if(any(phi <= 0) || abs(sum(phi)-1) > 1e-10) return(-Inf)
  lgamma(sum(xi)) - sum(lgamma(xi)) +
    sum( (xi-1) * log(phi) )
}
log_prior_rho <- function(rho, r) {
    if (rho < 0 || rho > 2 * r) {
        return(-Inf)
    } else {
        return(-log(2 * r))
    }
}
log_prior_W <- function(W, a_star, b_star,c_star, d_star) {
    dens <- dgbp(W, a_star, b_star, c_star, d_star)
    return(log(dens))
}

# Read the Gambia data
gambia_data <- read.csv(gambia, header = TRUE)

# Transform the data as needed
Y <- gambia_data$pos
X <- scale(gambia_data[, c(1, 2, 4:8)]) # Scale the covariates

# Create the random effects
s.ind <- numeric(length(Y))

for(i in 1:65){
  for(j in 1:length(Y)){
    if(X[j,2]==unique(X[,2])[i]){
      s.ind[j] = i
    }
  }
}

# Create spatial correlation matrix
X.loc <- unique(X[,1:2])

# Max distance
D_mat <- as.matrix(dist(X.loc))
r <- max(D_mat)

# Drop locations
X <- X[,-(1:2)]

n=length(Y) # number of data points
p=ncol(X) # number of fixed effects
q=1 # number of random effects
L=length(unique(s.ind))

# Determine mean Y
mean_y <- mean(Y)

# Logit link Y
logit_y <- log(Y / (1 - Y))

# Create the list of a b pairs to test
a_list <- c(0.5, 1, 1, 4, 4)
b_list <- c(0.5, 1, 4, 1, 4)

# Determine the parameters for the GBM prior
beta0_init <- qlogis(mean_y)

for (i in 1:length(a_list)){
    # Create the prior parameters
    a <- a_list[i]
    b <- b_list[i]

    # Determine parameters 
    params <- match_gbp(a, b, beta0 = beta0_init, family = "binomial")

    # Metropolis-Hastings sampler
    iterations <- 20000
    burn_in <- 2500
    adapt_interval <- 200
    p <- ncol(X)
    L <- length(unique(s.ind))

    # Create storage for the parameters
    draws <- list(beta = matrix(NA, iterations, p),
                beta0 = numeric(iterations),
                W = numeric(iterations),
                phi = matrix(NA, iterations, 2),
                rho = numeric(iterations),
                u = matrix(NA, iterations, L))

    # Set the initial values
    beta <- rep(0, p)
    beta0 <- 0
    W <- 1
    phi <- c(0.5, 0.5)
    rho <- runif(1, 0, 2 * r)
    Cmatrix <- exp(-D_mat / rho)
    u <- rep(0, L)

    # Set proposal standard deviations
    sd_beta <- rep(0.01, p)
    sd_beta0 <- 0.05
    sd_logW <- 0.1
    sd_logphi <- rep(0.2, 2)
    sd_logrho <- 0.05

    # Function to calculate the log posterior
    log_post <- function(beta, beta0, mu0, tau2, u, W, phi, rho, params, X, Y, X.loc, s.ind, r, D_mat) {
    C <- exp(-D_mat/rho)
    eta <- beta0 + X %*% beta + u[s.ind]
    p <- length(beta)
    ll <- logit_ll(Y, eta)
    lp <- log_prior_beta(beta,W,phi[1], p) +
            log_prior_beta0(beta0, mu0, tau2) +
            log_prior_u(u,W,phi[2],C) +
            log_prior_phi(phi) +
            log_prior_W(W, params[1], params[2], params[3], params[4]) +
            log_prior_rho(rho,r)
    ll + lp
    }

    # Initialize the log posterior
    cur_raw_lp <- log_post(beta, beta0, mu0 = 0, tau2 = 3, u, W, phi, rho, params, X, Y, X.loc, s.ind, r, D_mat)
    cur_full_lp <- cur_raw_lp

    # Performing iterations of algorithm using adaptive M-H
    for (i in 2:iterations) {
        # Update beta
        for(j in 1:p){
            prop <- beta
            prop[j] <- prop[j] + rnorm(1,0,sd_beta[j])
            prop_raw_lp <- log_post(prop, beta0, mu0 = 0, tau2 = 3, u, W, phi, rho, params, X, Y, X.loc, s.ind, r, D_mat)
            if(runif(1)<exp(prop_raw_lp - cur_full_lp)){
                beta <- prop
                cur_full_lp <- prop_raw_lp
                cur_raw_lp <- prop_raw_lp
            }
        }
        # Update beta0
        prop_b0 <- beta0 + rnorm(1,0,sd_beta0)
        prop_raw_lp <- log_post(beta, prop_b0, mu0 = 0, tau2 = 3, u, W, phi, rho, params, X, Y, X.loc, s.ind, r, D_mat)
        if(runif(1)<exp(prop_raw_lp - cur_full_lp)){
            beta0 <- prop_b0
            cur_full_lp <- prop_raw_lp
            cur_raw_lp <- prop_raw_lp
        }
        # Update W
        prop_logW <- log(W) + rnorm(1, 0, sd_logW)
        prop_W <- exp(prop_logW)
        prop_raw_lp <- log_post(beta, beta0, mu0 = 0, tau2 = 3, u, prop_W, phi, rho, params, X, Y, X.loc, s.ind, r, D_mat)
        prop_jacobian <- log(prop_W)
        cur_jacobian <- log(W)
        prop_full_lp <- prop_raw_lp + prop_jacobian
        cur_full_lp_temp <- cur_raw_lp + cur_jacobian
        if(runif(1)<exp(prop_full_lp - cur_full_lp_temp)){
            W <- prop_W
            cur_raw_lp <- prop_raw_lp
            cur_full_lp <- prop_full_lp
        }
        # Update phi
        logit_phi <- qlogis(phi)
        prop_logit <- logit_phi + rnorm(2,0,sd_logphi)
        prop_phi <- plogis(prop_logit)
        prop_phi <- prop_phi / sum(prop_phi)
        prop_raw_lp <- log_post(beta, beta0, mu0 = 0, tau2 = 3, u, W, prop_phi, rho, params, X, Y, X.loc, s.ind, r, D_mat)
        prop_jacobian <- sum(log(prop_phi)) + sum(log(1-prop_phi))
        cur_jacobian <- sum(log(phi)) + sum(log(1-phi))
        prop_full_lp <- prop_raw_lp + prop_jacobian
        if(runif(1)<exp(prop_full_lp - (cur_raw_lp + cur_jacobian))){
            phi <- prop_phi
            cur_raw_lp <- prop_raw_lp
            cur_full_lp <- prop_full_lp
        }
        # Update rho
        prop_logrho <- log(rho) + rnorm(1,0,sd_logrho)
        prop_rho <- exp(prop_logrho)
        prop_raw_lp <- log_post(beta, beta0, mu0 = 0, tau2 = 3, u, W, phi, prop_rho, params, X, Y, X.loc, s.ind, r, D_mat)
        prop_jacobian <- log(prop_rho)
        cur_jacobian <- log(rho)
        prop_full_lp <- prop_raw_lp + prop_jacobian
        cur_full_lp_temp <- cur_raw_lp + cur_jacobian
        if(runif(1)<exp(prop_full_lp - cur_full_lp_temp)){
            rho<-prop_rho
            cur_raw_lp <- prop_raw_lp
            cur_full_lp <- prop_full_lp
        }
        # Update u
        C <- exp(-D_mat/rho)
        Sigma_a <- W * phi[2] * C
        Q <- solve(Sigma_a)
        z_vec <- Y - 0.5 - (beta0 + X%*%beta)

        # Build vector of summed residuals by village
        g <- rowsum(z_vec, s.ind, reorder = FALSE)

        # Posterior precision and mean
        Gamma <- rowsum(Y-0.5, s.ind)
        Prec <- Q + t(G)%*%Omega%*%G
        mean_u <- solve(Prec, Gamma)
        u <- mean_u + backsolve(chol(Prec), rnorm(L))
        cur_raw_lp <- log_post(beta, beta0, mu0 = 0, tau2 = 3, u, W, phi, rho, params, X, Y, X.loc, s.ind, r, D_mat)
        cur_full_lp <- cur_raw_lp

        # Store the samples
        draws$beta[i, ] <- beta
        draws$beta0[i] <- beta0
        draws$W[i] <- W
        draws$phi[i, ] <- phi
        draws$rho[i] <- rho
        draws$u[i, ] <- u

    }

    # Remove burn-in samples
    keep <- (burn_in+1):iterations
    post_beta <- draws$beta[keep,]
    post_beta0 <- draws$beta0[keep]
    post_W <- draws$W[keep]
    post_rho <- draws$rho [keep]
    post_phi2 <- draws$phi[keep, 2]

    # Save the posterior samples to file
    outfile <- file.path(out_dir, paste0("gambia_", a, "_", b, "_", seed, ".rds"))
    saveRDS(list(beta_draws = post_beta,
                beta0_draws = post_beta0,
                W_draws = post_W,
                rho_draws = post_rho,
                phi2_draws = post_phi2,
                seed = seed,
                a = a,
                b = b),
            file = outfile)
    cat("Saved", outfile, "\n")
}