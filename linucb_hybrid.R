library(MASS)

update_A_0 <- function(A_0, B_rec, A_rec){
  A_rec_inv <- ginv(A_rec)
  New_A_0 <- A_0 + (t(B_rec) %*% A_rec_inv %*% B_rec)
  return(New_A_0)
}

update_b_0 <- function(b_0, B_rec, A_rec, b_rec){
  A_rec_inv <- ginv(A_rec)
  new_b_0 <- b_0 + (t(B_rec) %*% A_rec_inv %*% b_rec)
  return(new_b_0)
}

update_A_rec <- function(A_rec, x_t_rec){
  return(A_rec + x_t_rec %*% t(x_t_rec))
}

update_B_rec <- function(B_rec, x_t_rec, z_t_rec){
  return(B_rec + x_t_rec %*% t(z_t_rec))
}

update_b_rec <- function(b_rec, r_t, x_t_rec){
  return(b_rec + r_t * x_t_rec)
}

update_A_0_again <- function(A_0, z_t_rec, B_rec, A_rec){
  A_rec_inv <- ginv(A_rec)
  A_0_new <- A_0 + (z_t_rec %*% t(z_t_rec)) - (t(B_rec) %*% A_rec_inv %*% B_rec)
  return(A_0_new)
}

update_b_0_again <- function(b_0, r_t, z_t_rec, B_rec, A_rec, b_rec){
  A_rec_inv <- ginv(A_rec)
  new_b_0 <- b_0 +
    (r_t * z_t_rec) -
    (t(B_rec) %*% A_rec_inv %*% b_rec)
  return(new_b_0)
}
