library(dplyr)

source("utils_disjoint.R")

linucb_disjoint <- function(horizon, arms, delta, d){
  # Disjoint linear LinUCB model.
  #
  # Args:
  #   	horizon: number of trials.
  #	arms: number of arms (algorithms).
  #	delta: constant.
  #	d: dimension of instance features.
  #
  # Returns:
  alpha <- 1+sqrt(ln(2/delta)/2)
  arm_choice <- c()
  reward <- c()

  A <- list()
  b <- list()
  theta <- list()
  p <- matrix(0, horizon, arms)

  for (t in 1:horizon){
    for (a in arms){
      if (t == 1){
        A[[a]] <- diag(d)
	b[[a]] <- matrix(0, d, 1)
      }

    theta[[a]] <- compute_theta_a(b[[a]], A[[a]])
    x_t_a <- matrix(as.numeric(c(feature[t,], a)), d, 1)
    p[[t, a]] <- compute_p_t_a(x_t_a, A[[a]], theta[[a]], alpha)
    }

  arm_choice[t] <- which(p[t,] == max(p[t,]))
  reward[t] <- getRuntimes(...) # TODO

  x_t_a <- matrix(as.numeric(c(feature[t,], arm_choice[t])), d, 1)
  A[[arm_choice[t]]] <- update_A_a(A[[arm_choice[t]]], x_t_a)
  b[[arm_choice[t]]] <- update_b_a(b[[arm_choice[t]]], reward[t], x_t_a)

  return()
}
