# 05_gambia_supp.R  –  Performs MH sampling to estimate the posteriors from Gambia data
# Goal: This script reads the gambia dataset and recreates the MCMC sampling from the paper with slight alterations

# Workflow
#   1. Read the data
#   2. Transform the data as needed
#   3. Perform MCMC sampling to estimate the posteriors
#   4. Save the results to the output directory

# Usage:
#   Rscript 05_gambia_supp.R <gambia_data> <seed> <out_dir>


# Arguments:
#   - gambia_data: Path to the Gambia data file
#   - seed: Random seed for reproducibility
#   - out_dir: Directory to save sampling results

# Pull in the functions from the previous scripts
source("code/01_prior_construction.R")

# Load required libraries
library(rjags)

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

# Read the Gambia data
gambia_data <- read.csv(gambia, header = TRUE)

# Transform the data as needed
Y <- gambia_data$pos

# Get coordinates before scaling
coords <- gambia_data[, c("x", "y")]

# Build Factor of Village by finding unique pairs
loc_id <- with(gambia_data, paste(x, y, sep = "_"))
loc_fac <- factor(loc_id)

# Get interger indices for JAGS
s.ind <- as.integer(loc_fac)
L <- length(levels(loc_fac))

# The L by 2 matrix of unique village coordinates
X.loc <- coords[match(levels(loc_fac), loc_id), ]

# Compute max distance
r <- max(dist(X.loc))
cat("Max spatial distance among villages (r):", r, "\n")

# Now get scaled X values for features
X <- scale(gambia_data[, c("age", "netuse", "treated", "green", "phc")])

n=length(Y) # number of data points
p=ncol(X) # number of fixed effects
q=1 # number of random effects
L=length(unique(s.ind)) # number of locations
mean_Y <- mean(Y)
# Calculate Beta0 from the mean of Y
beta0 <- log(mean_Y/(1-mean_Y))
cat("Beta0:", beta0, "\n")

# Create the list of a b pairs to test
a_list <- c(0.5, 1, 1, 4, 4)
b_list <- c(0.5, 1, 4, 1, 4)
iterations <- 40000
burn_in <- 2500

for (h in 1:length(a_list)){
    cat("Starting sampling: a:", a_list[h], "b:", b_list[h], "\n")
    # Create the prior parameters
    a <- a_list[h]
    b <- b_list[h]

    # Determine parameters 
    params <- match_gbp(a, b, beta0 = beta0, family = "binomial")

    # Run code in JAGS copying the original code
    data = list(Y=Y, X=X, n=n, p=ncol(X), s.ind = s.ind, L=length(unique(s.ind)),
            X.loc = X.loc, params = params, r=r)

    model_string <- textConnection("model{

    ##Likelihood
    for(i in 1:n){
    Y[i] ~ dbern(1/(1+exp(-mu[i]-alpha[s.ind[i]])))
    mu[i] <- beta0 + inprod(X[i,],beta[])
    }

    alpha[1:L] ~ dmnorm.vcov(zero[1:L], dd[2]*W*C[1:L,1:L])

    ## Create correlation matrix

    for(i in 1:L){
    C[i,i] <- 1
    zero[i] <- 0
    }

    for(i in 1:(L-1)){
    for(j in (i+1):L){
        
        C[i,j] <- exp(-sqrt((X.loc[i,1] - X.loc[j,1])^2 + (X.loc[i,2] - X.loc[j,2])^2)/rho)
        C[j,i] <- exp(-sqrt((X.loc[i,1] - X.loc[j,1])^2 + (X.loc[i,2] - X.loc[j,2])^2)/rho)
    }
    }

    ##Priors

    V ~ dbeta(params[1], params[2])
    W <- (V/(1-V))^(1/params[3])*params[4]
    
    for(i in 1:2){delta[i] <- 1}
    dd ~ ddirch(delta)

    beta0 ~ dnorm(0,1/3)
    rho ~ dunif(0,2*r)
    for(j in 1:p){beta[j] ~ dnorm(0, p / (W * dd[1]))}

    }")

    inits <- list(beta=rep(0,p), alpha=rep(0,L))
    model <- jags.model(model_string,data = data, inits=inits, n.chains=1)
    update(model, 10000)
    params <- c("beta", "alpha", "beta0", "W", "dd", "rho")
    samples <- coda.samples(model,
                        variable.names=params,
                        n.iter=iterations)
    
    mat <- as.matrix(samples)
    # Remove burn-in
    mat <- mat[-(1:burn_in),]

    # Save the posterior samples to file
    outfile <- file.path(out_dir, paste0("gambia_", a, "_", b, "_", seed, ".rds"))
    saveRDS(
        list(
        beta0_draws = mat[, "beta0"],
        W_draws = mat[, "W"],
        dd1_draws = mat[, "dd[1]"],
        dd2_draws = mat[, "dd[2]"],
        rho_draws = mat[, "rho"]
        ),
    file = outfile)
    cat("Saved", outfile, "\n")
}