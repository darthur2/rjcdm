#' @useDynLib rjcdm, .registration = TRUE
#' @importFrom stats rgamma
#' @importFrom stats rnorm
#' @importFrom stats dnorm
#' @importFrom stats rbinom
#' @importFrom stats runif
#' @importFrom tmvtnorm rtmvnorm
#' @importFrom pgdraw pgdraw

#' @export
rjcdm <- function(Y,
                  Q, 
                  sigma2,
                  delta,
                  bounds, 
                  niter = 2000, 
                  nburn = 500,
                  M = 4){
  J <- ncol(Y)
  
  K_vec <- rowSums(Q)
  j_free <- which(K_vec > 1)

  A <- make_A(Q, M)
  
  m <- rep(1, J)
  m_free <- sample(1:M, length(j_free), TRUE)
  m[j_free] <- m_free
  
  Beta <- make_Beta(Q, m, "High")
  
  p <- rgamma(2^K, 1)
  p <- p/sum(p)
  
  Z <- sample_Z(Y, A, m, Beta, p)
  
  m_save <- matrix(0, niter-nburn, J)
  Beta_save <- vector("list", niter-nburn)
  Z_save <- matrix(0, niter-nburn, N)
  p_save <- matrix(0, niter-nburn, 2^K)
  
  for (iter in 1:niter){
    Omega <- sample_Omega(Z, A, Beta, m)

    Beta <- sample_Beta(Y, Z, A, m, Omega, Beta_prior, bounds)

    Z <- sample_Z(Y, A, m, Beta, p)

    p <- sample_p(Z, delta)
    
    m_out <- sample_m(Y, Z, A, Beta, Beta_prior, m, K_vec, pi_m, j_free, M)
    Beta <- m_out$Beta
    m <- m_out$m
    
    if (iter > nburn){
      m_save[iter-nburn,] <- m
      Beta_save[[iter-nburn]] <- Beta
      Z_save[iter-nburn,] <- apply(Z, 1, which.max)
      p_save[iter-nburn,] <- p
    }
  }
  
  m_hat <- apply(m_save, 2, function(x){
    which.max(tabulate(x, nbins = M))
  })
  
  Z_hat <- apply(Z_save, 2, function(x){
    which.max(tabulate(x, nbins = 2^K))
  })
  
  list(m_hat = m_hat,
       Z_hat = Z_hat)
}