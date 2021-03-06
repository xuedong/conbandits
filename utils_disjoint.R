# library(MASS)

##########################################
# Helper functions for matrices updating #
##########################################

update_A_a <- function(A_a, x_t_a){
  return(A_a + x_t_a %*% t(x_t_a))
}

update_b_a <- function(b_a, r_t, x_t_a){
  return(b_a + r_t * x_t_a)
}

##########################################################
# Helper functions for the main loop in disjoint version #
##########################################################

compute_theta_a <- function(b_a, A_a){
  return(solve(A_a, b_a))
}

compute_p_t_a <- function(x_t_a, A_a, theta_a, alpha0){
#  print("theta_a")
#  print(theta_a)
#  print("x_t_a")
#  print(x_t_a)
#  print("A_a")
#  print(A_a)
  p_t_a <- (t(theta_a) %*% x_t_a) + alpha0 * sqrt(t(x_t_a) %*% solve(A_a, x_t_a))
  return(p_t_a)
}
