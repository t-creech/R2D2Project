source("helpers.R")

# 1) Linear approx
approx_linear <- function(a, b, beta0, family) {
  # compute mu'(beta0) and sigma2(beta0) for given family
  mu_prime  <- mu_derivative(beta0, family)
  sigma2_0  <- sigma2(beta0, family)
  s2 <- sigma2_0 / mu_prime^2
  # returns GBP parameters (a, b, c=1, d=s2)
  list(a=a, b=b, c=1, d=s2)
}

# 2) QMC approx
approx_qmc <- function(a, b, beta0, family, K=200) {
  zs <- qnorm(seq(1/K, (K-1)/K, length.out=K-1))
  R2_tilde <- function(w) {
    etas <- beta0 + zs*sqrt(w)
    mus   <- mu_fn(etas, family)
    sig2s <- sigma2(etas, family)
    v_mu  <- var(mus)
    m_sig <- mean(sig2s)
    v_mu / (v_mu + m_sig)
  }
  # numeric derivative of R2_tilde
  dR2  <- function(w) numDeriv::grad(R2_tilde, w)
  # density
  dens <- function(w) {
    r2 <- R2_tilde(w)
    dbeta(r2, a, b) * abs(dR2(w))
  }
  dens
}

# 3) GBP‑fit via optimization
approx_gbp <- function(a, b, beta0, family, lambda=0.25) {
  # define “true” prior π(w) using approx_qmc or, if available, exact
  true_dens <- approx_qmc(a,b,beta0,family)
  # objective over α,β,c,d:
  #   ∫ [f_GBP(w;α,β,c,d) − true_dens(w)]²/true_dens(w) dw + λ*penalty
  # use optim() with bounds
  # return best (α,β,c,d)
}