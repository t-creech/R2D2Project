mh_algorithm <- function(
  n_samples,
  n_burnin,
  n_thin,
  proposal_sd,
  initial_value,
  log_likelihood_function,
  log_prior_function
) {
  # Initialize the chain
  samples <- numeric(n_samples)
  samples[1] <- initial_value
  current_value <- initial_value
  current_log_likelihood <- log_likelihood_function(current_value)
  current_log_prior <- log_prior_function(current_value)

  for (i in seq(2, n_samples)) {
    # Propose a new value
    proposed_value <- rnorm(1, mean = current_value, sd = proposal_sd)
    proposed_log_likelihood <- log_likelihood_function(proposed_value)
    proposed_log_prior <- log_prior_function(proposed_value)

    # Calculate the acceptance ratio
    acceptance_ratio <- exp(
      proposed_log_likelihood + proposed_log_prior -
        (current_log_likelihood + current_log_prior)
    )

    # Accept or reject the proposed value
    if (runif(1) < acceptance_ratio) {
      current_value <- proposed_value
      current_log_likelihood <- proposed_log_likelihood
      current_log_prior <- proposed_log_prior
    }

    # Store the sample if it's after burn-in and thinning
    if (i > n_burnin && (i - n_burnin) %% n_thin == 0) {
      samples[(i - n_burnin) / n_thin] <- current_value
    }
  }

  return(samples)
}