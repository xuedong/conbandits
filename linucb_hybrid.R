library(MASS)

##########################################
# Helper functions for matrices updating #
##########################################

update_A_0 <- function(A_0, B_a, A_a){
  A_a_inv <- ginv(A_a)
  A_0_new <- A_0 + (t(B_rec) %*% A_a_inv %*% B_a)
  return(A_0_new)
}

update_b_0 <- function(b_0, B_a, A_a, b_a){
  A_a_inv <- ginv(A_a)
  b_0_new <- b_0 + (t(B_a) %*% A_a_inv %*% b_a)
  return(b_0_new)
}

update_A_a <- function(A_a, x_t_a){
  return(A_a + x_t_a %*% t(x_t_a))
}

update_B_a <- function(B_a, x_t_a, z_t_a){
  return(B_a + x_t_a %*% t(z_t_a))
}

update_b_a <- function(b_a, r_t, x_t_a){
  return(b_a + r_t * x_t_a)
}

update_A_0_new <- function(A_0, z_t_a, B_a, A_a){
  A_a_inv <- ginv(A_a)
  A_0_new <- A_0 + (z_t_a %*% t(z_t_a)) - (t(B_a) %*% A_a_inv %*% B_a)
  return(A_0_new)
}

update_b_0_new <- function(b_0, r_t, z_t_a, B_a, A_a, b_a){
  A_a_inv <- ginv(A_a)
  b_0_new <- b_0 + (r_t * z_t_a) - (t(B_a) %*% A_a_inv %*% b_a)
  return(b_0_new)
}
