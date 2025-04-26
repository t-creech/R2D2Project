# code/01_prior_constructions.R
# This file contains helper functions to create estimates for 
# prior densities for W such that R2 ~ Beta(a, b)

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

va_der <- function(w, beta0, family, K = 1000) {
  z <- qnorm(seq(1, K-1) / K)
  sapply(w, function(wi) {
    if (wi <= 0) return(0)
    eta <- beta0 + sqrt(wi) * z
    mu  <- mu_fun(eta, family)
    var_mu     <- var(mu)
    mean_sigma <- mean(var_fun(eta, family))
    var_mu / (var_mu + mean_sigma)
  })
}

R2_to_W <- function(r2, beta0, family) {
  sapply(r2, function(r) {
    f <- function(logw) abs(va_der(exp(logw), beta0, family) - r)
    exp( optimise(f, c(log(1e-5), log(1e5)))$minimum )
  })
}

match_gbp <- function(a, b, beta0, lambda = 0.25, family){
    dR2_dw <- function(w, family) {
    delta <- 1e-3
    dr_dw <- (va_der(w + delta, beta0, family) - va_der(w - delta, beta0, family)) / (2 * delta)
    }
    tau   <- seq(0.01, 0.99, length.out = 100)
    r2_q  <- qbeta(tau, a, b)
    wgrid <- R2_to_W(r2_q, beta0, family)
    chi2 <- function(log_par){
        alpha <- exp(log_par[1]); beta <- exp(log_par[2]); c <- exp(log_par[3]); d <- exp(log_par[4])
        px <- c * (wgrid / d)^(alpha * c - 1) / (d * beta(alpha, beta) * (1 + (wgrid / d)^c)^(alpha + beta))
        py <- dbeta(r2_q, a, b) * abs(dR2_dw(wgrid, family))
        sum((1 - px / py)^2) + lambda * sum((log_par - log(c(a, b, 1, 1)))^2)
    }
    start <- log(c(a, b, 1, 1))
    opt <- optim(start, chi2, method = "Nelder-Mead")
    setNames(exp(opt$par), c("a_star", "b_star", "c_star", "d_star"))
}
