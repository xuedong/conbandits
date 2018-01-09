library(dplyr)
library(mlr)
library(llama)

source("utils_disjoint.R")

linucb_disjoint <- function(arms, instances, features, delta, scenario, getReward){
  # Disjoint linear LinUCB model.
  #
  # Args:
  #		arms: list of arms (algorithms).
  #		instances: list of available instances.
  #   features: features associated with instances.
  #		delta: constant.
  #   scenario: benchmark scenario.
  #   getReward: reward function.
  #
  # Returns:
  #   A, b
  #   armChoices: list of arm choices.
  #   rewards: list of rewards.
  alpha <- 1+sqrt(log(2/delta)/2)
  arm_choice <- c()
  reward <- c()
  number_arms <- length(arms)
  horizon <- length(instances)
  d <- length(features[1,])
  
  A <- list()
  b <- list()
  theta <- list()
  p <- matrix(0, horizon, number_arms)
  
  for (a in 1:number_arms){
    A[[a]] <- diag(d)
    b[[a]] <- matrix(0, d, 1)
  }
  
  for (t in 1:horizon){
    instance <- instances[t]
    for (a in 1:number_arms){
      theta[[a]] <- compute_theta_a(b[[a]], A[[a]])
      x_t_a <- matrix(as.numeric(c(features[t,2:d], a)), d, 1)
      p[[t, a]] <- compute_p_t_a(x_t_a, A[[a]], theta[[a]], alpha)
    }
    
    arm_choice[t] <- which(p[t,] == max(p[t,]))
    reward[t] <- getReward(arms[arm_choice[t]], instance, scenario)$runtime
    
    x_t_a <- matrix(as.numeric(c(features[t,2:d], arm_choice[t])), d, 1)
    A[[arm_choice[t]]] <- update_A_a(A[[arm_choice[t]]], x_t_a)
    b[[arm_choice[t]]] <- update_b_a(b[[arm_choice[t]]], reward[t], x_t_a)
  }
  
  return(list("A"=A, "b"=b, "armChoices"=arm_choice, "rewards"=reward))
}
