sample_m <- function(Y,
                     Z,
                     A,
                     Beta,
                     Beta_prior,
                     m,
                     bounds,
                     K_vec,
                     pi_m,
                     j_free, 
                     M){
  
  for (j in j_free){
    m_j_out <- sample_m_j(Y,
                          Z,
                          A,
                          Beta,
                          Beta_prior,
                          m,
                          K_vec,
                          pi_m, 
                          bounds,
                          j)
    Beta[[j]] <- m_j_out$beta_j
    m[j] <- m_j_out$m_j
  }
  
  list(Beta = Beta,
       m = m)
}

sample_m_j <- function(Y,
                       Z,
                       A,
                       Beta,
                       Beta_prior,
                       m,
                       K_vec,
                       pi_m, 
                       bounds,
                       j, M = 4){
  
  Y_j <- Y[,j]
  beta_j <- Beta[[j]]
  m_j <- m[j]
  pi_m_j <- pi_m[m_j]
  K_j <- K_vec[j]
  mu_m_j <- Beta_prior[[m_j]][[j]]$mu_m_j
  sigma_m_j <- Beta_prior[[m_j]][[j]]$sigma_m_j
  lower_m_j <- bounds[[m_j]][[j]]$lower_j
  upper_m_j <- bounds[[m_j]][[j]]$upper_j
  
  m_j_star <- sample(1:M, 1, prob = pi_m)
  pi_m_j_star <- pi_m[m_j_star]
  
  mu_m_j_star <- Beta_prior[[m_j_star]][[j]]$mu_m_j
  sigma_m_j_star <- Beta_prior[[m_j_star]][[j]]$sigma_m_j
  lower_m_j_star <- bounds[[m_j_star]][[j]]$lower_j
  upper_m_j_star <- bounds[[m_j_star]][[j]]$upper_j
  
  u_j_out <- sample_u_j(beta_j, m_j, m_j_star, K_j)
  
  u_j_star <- u_j_out$u_j_star
  beta_j_star <- u_j_out$beta_j_star
  u_j <- u_j_out$u_j
  
  ld_u_j_out <- comp_ld_u_j(u_j, u_j_star, m_j, m_j_star, K_j)
  
  ld_u_j_star <- ld_u_j_out$ld_u_j_star
  ld_u_j <- ld_u_j_out$ld_u_j
  
  A_m_j <- A[[m_j]][[j]]
  A_m_j_star <- A[[m_j_star]][[j]]
  
  lp_m_j <- comp_lp_m_j(Y_j,
                        Z,
                        A_m_j,
                        beta_j,
                        mu_m_j,
                        sigma_m_j,
                        pi_m_j,
                        m_j, 
                        lower_m_j,
                        upper_m_j,
                        K_j)
  
  lp_m_j_star <- comp_lp_m_j(Y_j,
                             Z,
                             A_m_j_star,
                             beta_j_star,
                             mu_m_j_star,
                             sigma_m_j_star,
                             pi_m_j_star,
                             m_j_star,
                             lower_m_j_star,
                             upper_m_j_star,
                             K_j)
  
  log_accept <- (lp_m_j_star + ld_u_j) - (lp_m_j + ld_u_j_star)

  if (log(stats::runif(1)) < log_accept){
    beta_j <- beta_j_star
    m_j <- m_j_star
  }
  
  list(beta_j = beta_j,
       m_j = m_j)
}

sample_Beta <- function(Y,
                        Z,
                        A,
                        m,
                        Omega,
                        Beta,
                        Beta_prior,
                        bounds){
  
  J <- ncol(Y)
  
  for (j in 1:J){
    Beta[[j]] <- sample_beta_j(Y, Z, A, m, Omega, Beta_prior, bounds, j)
  }
  
  Beta
}

sample_beta_j <- function(Y,
                          Z,
                          A,
                          m,
                          Omega,
                          Beta_prior,
                          bounds,
                          j){
  Y_j <- Y[,j]
  kappa_j <- Y_j - 1/2
  m_j <- m[j]
  A_m_j <- A[[m_j]][[j]]
  lower_j <- bounds[[m_j]][[j]]$lower_j
  upper_j <- bounds[[m_j]][[j]]$upper_j
  mu_m_j <- Beta_prior[[m_j]][[j]]$mu_m_j
  sigma_m_j <- Beta_prior[[m_j]][[j]]$sigma_m_j
  I_sigma2 <- diag(1/sigma_m_j^2)
  
  A_tilde_m_j <- Z%*%A_m_j
  
  weighted_A_m_j <- A_tilde_m_j * Omega[, j]
  Sigma_j <- crossprod(A_tilde_m_j, weighted_A_m_j) + I_sigma2
  chol_Sigma_j <- chol(Sigma_j)
  Sigma_j <- chol2inv(chol_Sigma_j)
  
  rhs_j <- t(A_tilde_m_j) %*% kappa_j + I_sigma2 %*% mu_m_j
  mu_star_j <- c(backsolve(chol_Sigma_j, forwardsolve(t(chol_Sigma_j), rhs_j)))
  
  beta_j <- c(tmvtnorm::rtmvnorm(1, mu_star_j, Sigma_j, lower_j, upper_j))

  beta_j
}

sample_Omega <- function(Z, A, Beta, m){
  N <- nrow(Z)
  J <- length(Beta)
  
  Omega <- matrix(0, N, J)
  
  for (j in 1:J){
    m_j <- m[j]
    A_m_j <- A[[m_j]][[j]]
    
    c_j <- Z%*%(A_m_j%*%Beta[[j]])
    
    Omega[,j] <- pgdraw::pgdraw(1, c_j)
  }
  
  Omega
}

sample_Z <- function(Y, A, m, Beta, p){
  Z <- sample_Z_cpp(Y, A, m, Beta, p)
  
  Z
}

sample_p <- function(Z , delta){
  CC <- length(delta)
  
  delta_star <- colSums(Z) + delta
  
  x <- stats::rgamma(CC, delta_star)
  
  p <- x/sum(x)
  
  p
}
