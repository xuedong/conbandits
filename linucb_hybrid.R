library(dplyr)
library(mlr)
library(llama)

sourceDir <- function(path, trace = TRUE, ...) {
	for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
		if(trace) cat(nm,":")
		source(file.path(path, nm), ...)
	    if(trace) cat("\n")
	}
}
sourceDir("helpFunctions")
source("utils_hybrid.R")

linucb_hybrid <- function(horizon, arms, delta, d, k){
	# Hybrid linear LinUCB model.
  	#
  	# Args:
  	# 		horizon: number of trials.
  	#		arms: number of arms (algorithms).
  	#		delta: constant.
  	#		d: dimension of instance features.
  	#		k: dimension of shared features.
  	#
  	# Returns:
  	alpha <- 1+sqrt(ln(2/delta)/2)
  	arm_choice <- c()
  	reward <- c()

  	A_0 <- diag(k)
  	b_0 <- matrix(0, k, 1)
  	A <- list()
  	B <- list()
  	b <- list()
  	theta <- list()
  	s <- matrix(0, horizon, arms)
  	p <- matrix(0, horizon, arms)

  	for (t in 1:horizon){
    	beta <- ginv(A_0) %*% b_0

		for (a in arms){
      		if (t == 1){
        		A[[a]] <- diag(d)
  				B[[a]] <- matrix(0, d, k)
				b[[a]] <- matrix(0, d, 1)
      		}

    		theta[[a]] <- compute_theta_a(b[[a]], A[[a]], B[[a]], beta)
    		x_t_a <- matrix(as.numeric(c(feature[t,], a)), d, 1)
    		z_t_a <- matrix(as.numeric(c(feature[t,], shared[a,])), k, 1)
    		s[[t, a]] <- compute_s_t_a(z_t_a, A_0, B[[a]], A[[a]], x_t_a)
    		p[[t, a]] <- compute_p_t_a(z_t_a, x_t_a, s[[t, a]], beta, theta[[a]], alpha)
    	}
 
  		arm_choice[t] <- which(p[t,] == max(p[t,]))
  		reward[t] <- getRuntimes(...) # TODO Replace the reward function by getRuntimes  

  		x_t_a <- matrix(as.numeric(c(feature[t,], arm_choice[t])), d, 1)
  		z_t_a <- matrix(as.numeric(c(feature[t,], shared[arm_choice[t],])), k, 1)

  		A_0 <- update_A_0(A_0, B[[arm_choice[t]]], A[[arm_choice[t]]])
  		b_0 <- update_b_0(b_0, B[[arm_choice[t]]], A[[arm_choice[t]]], b[[arm_choice[t]]])
  		A[[arm_choice[t]]] <- update_A_a(A[[arm_choice[t]]], x_t_a)
  		B[[arm_choice[t]]] <- update_B_a(B[[arm_choice[t]]], x_t_a, z_t_a)
  		b[[arm_choice[t]]] <- update_b_a(b[[arm_choice[t]]], reward[t], x_t_a)
  		A_0 <- update_A_0_new(A_0, z_t_a, B[[arm_choice[t]]], A[[arm_choice[t]]])
  		B_0 <- update_b_0_new(b_0, reward[t], z_t_a, B[[arm_choice[t]]], A[[arm_choice[t]]], b[[arm_choice[t]]])
	}
 
   	return()
}
