library(dplyr)
library(mlr)
library(llama)

linucb_step <- function(D, p, arms, instance, feature, alpha0, scenario, getReward){
  # LinUCB with an D and p.
  #
  # Args:
  #   D: list of design matrices for each arm, 
  #     each matrix would be of size m*d 
  #     where m is the current number of instances
  #     passed, d is the number of features.
  #   p: vector of predicted values of
  #     each uderlying mlr regression model
  #     given the new feature.
  #		arms: list of arms (algorithms).
  #		instance: new arriving instance.
  #   feature: feature associated with this instance.
  #		alpha0: constant.
  #   scenario: benchmark scenario.
  #   getReward: reward function.
  #   nb: current number of instances treated.
  #
  # Returns:
  #   armChoice: arm choosen in this step.
  #   reward: reward (performance) obtained in this step.
  number_arms <- length(arms)
  d <- length(feature)
  # counter <- nb + 1
  
  predicted <- matrix(0, 1, number_arms)
  
  for (a in 1:number_arms){
    x_t_a <- matrix(as.numeric(c(feature[2:d])), d-1, 1)
    A_a <- t(D[[a]]) %*% D[[a]] + diag(d-1)
    predicted[[1, a]] <- p[a] + alpha0 * sqrt(t(x_t_a) %*% ginv(A_a) %*% x_t_a)
  }
    
  trial_arm <- which(predicted[1,] == max(predicted[1,]))
  if (length(trial_arm) > 1){
    trial_arm <- sample(trial_arm, 1)
    arm_choice <- trial_arm
  }
  else{
    arm_choice <- trial_arm
  }
  reward <- getReward(arms[arm_choice], instance, scenario)$performance
  
  return(list("reward"=reward, "armChoice"=arm_choice))
}