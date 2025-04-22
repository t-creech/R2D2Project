# code/03_exact_prior_constructions.R
# Closed‑form prior densities for W such that R2 ~ Beta(a, b)

# Beta prime prior
prior_BP <- function(w, a, b) {
    B_ab <- beta(a, b)
    density <- (w^(a - 1) * (1 + w)^(-a - b)) / B_ab
    density[w < 0] <- 0
    density[w > 1] <- 0
    return(density)
}

# Poisson regression prior
prior_GBP_poisson <- function(w, a, b, beta0) {
    B_ab <- beta(a, b)
    num <- (exp(w) - 1)^(a-1) * exp(-b * (beta0 + w/2)) * (3*exp(w) - 1) / 2
    denom <- (exp(w) - 1 + exp(-beta0 - w/2))^(a + b)
    density <- num / (B_ab * denom)
    density[w < 0] <- 0
    density[w > 1] <- 0
    return(density)
}

