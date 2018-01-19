library(dplyr)
library(mlr)
library(llama)

source("utils_hybrid.R")

linucb_hybrid <- function(arms, instances, features, shared, alpha, scenario, getReward, nb){
  # Hybrid linear LinUCB model.
  #
  # Args:
  #		arms: list of arms (algorithms).
  #		instances: list of available instances.
  #   features: features associated with instances.
  #		alpha: constant (may be set to be an increasing function on the horizon).
  #   scenario: benchmark scenario.
  #   getReward: reward function.
  #   nb: current number of instances treated.
  #
  # Returns:
  #   A, B, b
  #   armChoices: list of arm choices.
  #   rewards: list of rewards.
  #   counter: number of instances treated after the call of this function.
  arm_choice <- c()
  reward <- c()
  number_arms <- length(arms)
  horizon <- length(instances)
  d <- length(features[1,])
  k <- length(shared[1,])
  counter <- nb + horizon
  
  A_0 <- diag(k)
  b_0 <- matrix(0, k, 1)
  A <- list()
  B <- list()
  b <- list()
  theta <- list()
  s <- matrix(0, horizon, number_arms)
  p <- matrix(0, horizon, number_arms)
  
  for (a in 1:number_arms){
    A[[a]] <- diag(d)
    B[[a]] <-  matrix(0, d, k)
    b[[a]] <- matrix(0, d, 1)
  }
  
  for (t in 1:horizon){
    instance <- instances[t]
    beta <- ginv(A_0) %*% b_0
    
    for (a in arms){
      theta[[a]] <- compute_theta_a(b[[a]], A[[a]], B[[a]], beta)
      x_t_a <- matrix(as.numeric(c(features[t, 2:d], a)), d, 1)
      z_t_a <- matrix(as.numeric(c(features[t, 2:d], shared[a,])), k, 1)
      s[[t, a]] <- compute_s_t_a(z_t_a, A_0, B[[a]], A[[a]], x_t_a)
      p[[t, a]] <- compute_p_t_a(z_t_a, x_t_a, s[[t, a]], beta, theta[[a]], alpha)
    }
    
    arm_choice[t] <- which(p[t,] == max(p[t,]))
    reward[t] <- getRuntimes(arms[arm_choice[t]], instance, scenario)$runtime
    
    x_t_a <- matrix(as.numeric(c(feature[t, 2:d], arm_choice[t])), d, 1)
    z_t_a <- matrix(as.numeric(c(feature[t, 2:d], shared[arm_choice[t],])), k, 1)
    
    A_0 <- update_A_0(A_0, B[[arm_choice[t]]], A[[arm_choice[t]]])
    b_0 <- update_b_0(b_0, B[[arm_choice[t]]], A[[arm_choice[t]]], b[[arm_choice[t]]])
    A[[arm_choice[t]]] <- update_A_a(A[[arm_choice[t]]], x_t_a)
    B[[arm_choice[t]]] <- update_B_a(B[[arm_choice[t]]], x_t_a, z_t_a)
    b[[arm_choice[t]]] <- update_b_a(b[[arm_choice[t]]], reward[t], x_t_a)
    A_0 <- update_A_0_new(A_0, z_t_a, B[[arm_choice[t]]], A[[arm_choice[t]]])
    B_0 <- update_b_0_new(b_0, reward[t], z_t_a, B[[arm_choice[t]]], A[[arm_choice[t]]], b[[arm_choice[t]]])
  }
  
  return(list("A"=A, "B"=B, "b"=b, "armChoices"=arm_choice, "rewards"=reward, "counter"=counter))
}
