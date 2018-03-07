library(dplyr)
library(mlr)
library(llama)

source("utils_disjoint.R")

linucb_disjoint <- function(arms, instances, features, alpha0, scenario, getReward, nb){
  # Disjoint linear LinUCB model.
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
  #   A, b, p
  #   armChoices: list of arm choices.
  #   rewards: list of rewards.
  #   counter: number of instances treated after the call of this function.
  arm_choice <- c()
  reward <- c()
  number_arms <- length(arms)
  horizon <- length(instances)
  d <- length(features[1,])
  counter <- nb + horizon
  
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
      x_t_a <- matrix(as.numeric(c(features[t, 2:d], a)), d, 1)
      p[[t, a]] <- compute_p_t_a(x_t_a, A[[a]], theta[[a]], alpha0)
    }
    
    trial_arm <- which(p[t,] == max(p[t,]))
    if (length(trial_arm) > 1){
      trial_arm <- sample(trial_arm, 1)
      arm_choice[t] <- trial_arm
    }
    else{
      arm_choice[t] <- trial_arm
    }
    reward[t] <- getReward(arms[arm_choice[t]], instance, scenario)$performance
    
    x_t_a <- matrix(as.numeric(c(features[t, 2:d], arm_choice[t])), d, 1)
    A[[arm_choice[t]]] <- update_A_a(A[[arm_choice[t]]], x_t_a)
    b[[arm_choice[t]]] <- update_b_a(b[[arm_choice[t]]], reward[t], x_t_a)
  }
  
  return(list("A"=A, "b"=b, "armChoices"=arm_choice, "rewards"=reward, "counter"=counter))
}

linucb_initialization <- function(arms, instances, features, scenario, getReward, nb){
  # Initialization of A and b with full informatioin.
  #
  # Args:
  #   arms
  #   instances
  #   features
  #   nb
  #
  # Returns:
  #   A and b
  #   counter
  number_arms <- length(arms)
  horizon <- length(instances)
  d <- length(features[1,])
  counter <- nb + horizon
  
  A <- list()
  b <- list()
  
  for (a in 1:number_arms){
    A[[a]] <- diag(d-1)
    b[[a]] <- matrix(0, d-1, 1)
  }
  
  for (t in 1:horizon){
    instance <- instances[t]
    for (a in 1:number_arms){
      x_t_a <- matrix(as.numeric(c(features[t, 2:d])), d-1, 1)
      A[[a]] <- update_A_a(A[[a]], x_t_a)
      reward <- getReward(arms[a], instance, scenario)$performance
      b[[a]] <- update_b_a(b[[a]], reward, x_t_a)
    }
  }
  
  return(list("A"=A, "b"=b, "counter"=counter))
}

linucb_disjoint_update <- function(A, b, arms, instances, features, alpha0, scenario, getReward, nb){
  # LinUCB with an initial A and b.
  #
  # Args:
  #   Initial A, b and p
  #		arms: list of arms (algorithms).
  #		instances: list of available instances.
  #   features: features associated with instances.
  #		alpha: constant.
  #   scenario: benchmark scenario.
  #   getReward: reward function.
  #   nb: current number of instances treated.
  #
  # Returns:
  #   New A and b
  #   armChoices: list of arm choices.
  #   rewards: list of rewards.
  #   counter: number of instances treated after the call of this function.
  arm_choice <- c()
  reward <- c()
  number_arms <- length(arms)
  horizon <- length(instances)
  d <- length(features[1,])
  counter <- nb + horizon
  
  theta <- list()
  p <- matrix(0, horizon, number_arms)
  
  for (t in 1:horizon){
    instance <- instances[t]
    for (a in 1:number_arms){
      theta[[a]] <- compute_theta_a(b[[a]], A[[a]])
      x_t_a <- matrix(as.numeric(c(features[t,2:d], a)), d, 1)
      p[[t, a]] <- compute_p_t_a(x_t_a, A[[a]], theta[[a]], alpha0)
    }
    
    trial_arm <- which(p[t,] == max(p[t,]))
    if (length(trial_arm) > 1){
      trial_arm <- sample(trial_arm, 1)
      arm_choice[t] <- trial_arm
    }
    else{
      arm_choice[t] <- trial_arm
    }
    reward[t] <- getReward(arms[arm_choice[t]], instance, scenario)$performance
    
    x_t_a <- matrix(as.numeric(c(features[t,2:d], arm_choice[t])), d, 1)
    A[[arm_choice[t]]] <- update_A_a(A[[arm_choice[t]]], x_t_a)
    b[[arm_choice[t]]] <- update_b_a(b[[arm_choice[t]]], reward[t], x_t_a)
  }

  return(list("A"=A, "b"=b, "armChoices"=arm_choice, "rewards"=reward, "counter"=counter))
}

linucb_predict <- function(A, b, arms, instances, features, scenario, getReward){
  # Prediction with only the underlying LinUCB regression.
  #
  # Args:
  #   A, b
  #		arms: list of arms (algorithms).
  #		instances: list of available instances.
  #   features: features associated with instances.
  #   scenario: benchmark scenario.
  #   getReward: reward function.
  #
  # Returns:
  #   armChoices: list of arm choices (predictions).
  #   rewards: list of rewards.
  arm_choice <- c()
  reward <- c()
  number_arms <- length(arms)
  horizon <- length(instances)
  d <- length(features[1,])
  
  theta <- list()
  p <- matrix(0, horizon, number_arms)
  
  for (t in 1:horizon){
    instance <- instances[t]
    for (a in 1:number_arms){
      theta[[a]] <- compute_theta_a(b[[a]], A[[a]])
      x_t_a <- matrix(as.numeric(c(features[t,2:d])), d-1, 1)
      p[[t, a]] <- compute_p_t_a(x_t_a, A[[a]], theta[[a]], 0)
    }
    
    trial_arm <- which(p[t,] == max(p[t,]))
    if (length(trial_arm) > 1){
      trial_arm <- sample(trial_arm, 1)
      arm_choice[t] <- trial_arm
    }
    else{
      arm_choice[t] <- trial_arm
    }
    reward[t] <- getReward(arms[arm_choice[t]], instance, scenario)$performance
  }
  
  return(list("armChoices"=arm_choice, "rewards"=reward))
}
