# code/01_prior_constructions.R
# This file contains helper functions to create estimates for 
# prior densities for W such that R2 ~ Beta(a, b)

# Function to calculate the mean of the linear predictor for different families
mu_fun <- function(eta, family) {
    if (family == "gaussian") {
        return(eta)
    } else if (family == "poisson") {
        return(exp(eta))
    } else if (family == "binomial") {
        return(plogis(eta))
    } else {
        stop("Unsupported family")
    }
}

# Function to calculate the variance of the mean for different families
var_fun <- function(eta, family) {
    if (family == "gaussian") {
        return(rep(1, length(eta)))
    } else if (family == "poisson") {
        return(exp(eta))
    } else if (family == "binomial") {
        mu <- plogis(eta)
        return(mu * (1 - mu))
    } else {
        stop("Unsupported family")
    }
}

# Function to calculate the variance of the posterior distribution 
va_der <- function(w, beta0, family, K = 1000) {
    if (family == "gaussian") {
        return(w / (w + 1))
    } else if (family == "poisson") {
        return((exp(w) - 1) / (exp(w) - 1 + exp(-beta0 - 0.5 * w)))
    } else if (family == "binomial") {
        z <- qnorm(seq(1, K-1) / K)
        sapply(w, function(wi) {
            if (wi <= 0) return(0)
            eta <- beta0 + sqrt(wi) * z
            mu <- plogis(eta)
            M <- mean(mu^2) - mean(mu)^2
            V <- mean(mu * (1 - mu))
            return(M / (M + V))
        })
    } else {
        stop("Unsupported family")
    }
}

# Function to convert R2 to W
R2_to_W <- function(r2, beta0, family) {
    sapply(r2, function(r) {
        f <- function(logw) abs(va_der(exp(logw), beta0, family) - r)
        exp( optimise(f, c(log(1e-5), log(1e5)))$minimum )
    })
}

# Function to convert W to R2
W_to_R2 <- function(W, beta0 = 0, family){
    va_der(W, beta0, family) # VaDer is basically the inverse of R2_to_W
}

# Function to convert quantiles of R2 to quantiles of W
qw <- function(p, a = 1, b = 1, b0 = 0, family){
  sapply(p, function(tau) {
    r2 <- qbeta(tau, a, b)
    R2_to_W(r2, beta0 = b0, family = family)
  })
}

# Function to convert quantiles of W to quantiles of R2
pw <- function(w, a = 1, b = 1, b0 = 0, family){
    r2 <- W_to_R2(w, beta0 = b0, family = family)
    pbeta(r2, a, b)
}


dw <- function(w, a = 1, b = 1, b0 = 0, family, delta = 1e-4){
    r2  <- W_to_R2(w, beta0 = b0, family = family)
    jac <- abs(dR2dw(w, beta0 = b0, family = family, delta = delta))
    dbeta(r2, a, b) * jac
}

# Function to find linear approximations of gbp parameters
linear_gbp_params <- function(a, b, beta0, family){
    mu1 <- switch(family,
                gaussian = 1,
                poisson = exp(beta0),
                binomial = plogis(beta0)*(1-plogis(beta0)))
    sigma2 <- mean(var_fun(beta0, family))
    s2 <- sigma2 / mu1^2
    setNames(c(a, b, 1, s2), c("a_star", "b_star", "c_star", "d_star"))
}

# Function to calculate the derivative of R2 with respect to W
dR2dw <- function(w, beta0, family, delta = 1e-4) {
    (va_der(w+delta, beta0, family) - va_der(pmax(0,w-delta), beta0, family)) /(2*delta)
}

# Function to match the generalized beta prime distribution to a given beta distribution
match_gbp <- function(a, b, beta0, lambda = 0.25, family){
    tau <- seq(0.01, 0.99, length.out = 100)
    wgrid <- qw(tau, a, b, b0 = beta0, family = family)
    py <- dw(w = wgrid, a = a, b = b, b0 = beta0, family = family)
    wgrid <- wgrid[!is.na(py)]
    py <- py[!is.na(py)]
    chi2 <- function(logparam){
        alpha <- exp(logparam[1])
        beta <- exp(logparam[2]) 
        c <- exp(logparam[3])
        d <- exp(logparam[4])
        px <- sapply(wgrid, function(w) {dgbp(w, alpha, beta, c, d)})
        divergence <- sum((1 - px/py)^2)
        penalty <- lambda * ((logparam[1] - log(a))^2 + (logparam[2] - log(b))^2 + (logparam[3] - log(1))^2 + (logparam[4] - log(1))^2)
        return(divergence + penalty)
    }
    opt <- optim(par = c(log(a), log(b), log(1), log(1)), fn = chi2, method = "Nelder-Mead")
    vals <- exp(opt$par)
    return(setNames(vals, c("a_star", "b_star", "c_star", "d_star")))
}

# Function to calculate the density of the generalized beta prime distribution in exact cases
exact_r2d2_prior <- function(w, a, b, beta0, family){
    if (family == "gaussian") {
        return((1 / beta(a, b)) * (w^(a - 1) / (1 + w)^(a + b)))
    } else if (family == "poisson") {
        return((exp(w) - 1)^(a - 1) * exp(-b * (beta0 + w / 2)) * (3 * exp(w) - 1) / (beta(a, b) * 2 * (exp(w) - 1 + exp(-beta0 - w / 2))^(a + b)))
    } else {
        stop("Family with no closed form solution")
    }
}

# Function to calculate the density of the generalized beta prime distribution given a, b, c, d
dgbp <- function(w, a, b, c, d){
  c * (w/d)^(a*c - 1) / ( d * beta(a, b) * (1 + (w/d)^c)^(a + b) )
}