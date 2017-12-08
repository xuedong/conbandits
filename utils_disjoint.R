library(MASS)

##########################################
# Helper functions for matrices updating #
##########################################

update_A_a <- function(A_a, x_t_a){
  return(A_a + x_t_a %*% t(x_t_a))
}

update_b_a <- function(b_a, r_t, x_t_a){
  return(b_a + r_t * x_t_a)
}
