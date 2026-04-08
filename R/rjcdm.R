#' @useDynLib rjcdm, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom stats rgamma
#' @importFrom stats rnorm
#' @importFrom stats dnorm
#' @importFrom stats rbinom
#' @importFrom stats runif
#' @importFrom stats pnorm
#' @importFrom utils combn
#' @importFrom tmvtnorm rtmvnorm
#' @importFrom pgdraw pgdraw
NULL

#' Reversible Jump MCMC for Cognitive Diagnosis Models (RJCDM)
#'
#' Fits a Bayesian cognitive diagnosis model (CDM) using a reversible jump
#' Markov chain Monte Carlo (RJMCMC) algorithm. The function estimates latent
#' classes, item parameters, and item-specific model structures simultaneously.
#'
#' Each item is allowed to switch between multiple CDM specifications (e.g.,
#' G-DINA, ACDM, DINA, DINO), enabling flexible modeling of item-response
#' relationships and automatic model selection.
#'
#' @details
#' Let \eqn{Y} be a binary response matrix of size \eqn{N \times J}. The model assumes:
#'
#' \deqn{
#' y_{ij} \sim \text{Bernoulli}(\theta_{ij})
#' }
#'
#' where \eqn{\theta_{ij}} depends on:
#' \itemize{
#'   \item Latent class membership \eqn{z_i}
#'   \item Item-specific parameters \eqn{\beta_j^{(m_j)}}
#'   \item Model indicator \eqn{m_j}
#' }
#'
#' Latent class memberships follow a multinomial distribution:
#' \deqn{
#' \mathbf{z}_i \sim \text{Multinomial}(1, \mathbf{p})
#' }
#'
#' with class probabilities:
#' \deqn{
#' \mathbf{p} \sim \text{Dirichlet}(\boldsymbol{\delta})
#' }
#'
#' Item parameters are assigned truncated normal priors:
#' \deqn{
#' \boldsymbol{\beta}_j^{(m_j)} \sim \mathcal{TN}(\mu^{(m_j)}, \sigma^{(m_j)})
#' }
#'
#' Model indicators are sampled via reversible jump MCMC:
#' \deqn{
#' m_j \sim \text{Categorical}(\boldsymbol{\pi})
#' }
#'
#' Posterior sampling uses:
#' \itemize{
#'   \item Polya-Gamma augmentation for logistic likelihoods
#'   \item Gibbs updates for \eqn{\beta}, \eqn{Z}, and \eqn{p}
#'   \item Reversible jump steps for model indicators \eqn{m_j}
#' }
#'
#' @param Y A binary response matrix of size \eqn{N \times J}, where rows
#'   correspond to individuals and columns to items.
#'
#' @param Q A binary \eqn{J \times K} Q-matrix indicating which attributes
#'   are required by each item.
#'
#' @param delta A vector of length \eqn{2^K} specifying Dirichlet prior
#'   parameters for latent class probabilities.
#'
#' @param Beta_prior A nested list containing prior means and standard
#'   deviations for item parameters under each model.
#'
#' @param bounds A nested list specifying lower and upper truncation bounds
#'   for item parameters under each model.
#'
#' @param pi_m A vector of length \eqn{M} giving prior probabilities over
#'   candidate models.
#'
#' @param niter Total number of MCMC iterations (default = 2000).
#'
#' @param nburn Number of burn-in iterations to discard (default = 500).
#'
#' @param M Number of candidate CDM models (default = 4).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{m_hat}: Posterior mode of the model indicator for each item
#'   \item \code{Z_hat}: Posterior mode of the latent class for each individual
#' }
#'
#' @section Algorithm:
#' Each iteration of the sampler performs:
#' \enumerate{
#'   \item Sample Polya-Gamma latent variables
#'   \item Sample item parameters \eqn{\beta}
#'   \item Sample latent class assignments \eqn{Z}
#'   \item Sample class probabilities \eqn{p}
#'   \item Update model indicators \eqn{m} via RJMCMC
#' }
#'
#' @section Model Space:
#' The function supports multiple CDM specifications:
#' \itemize{
#'   \item 1: G-DINA
#'   \item 2: ACDM
#'   \item 3: DINA
#'   \item 4: DINO
#' }
#'
#' @examples
#' \dontrun{
#' fit <- rjcdm(
#'   Y = Y,
#'   Q = Q,
#'   delta = rep(1, 2^K),
#'   Beta_prior = Beta_prior,
#'   bounds = bounds,
#'   pi_m = rep(1/4, 4),
#'   niter = 2000,
#'   nburn = 500
#' )
#'
#' fit$m_hat
#' fit$Z_hat
#' }
#'
#' @export
rjcdm <- function(Y,
                  Q, 
                  delta,
                  Beta_prior,
                  bounds,
                  pi_m,
                  niter = 2000, 
                  nburn = 500,
                  M = 4){
  N <- nrow(Y)
  J <- ncol(Y)
  K <- ncol(Q)
  
  K_vec <- rowSums(Q)
  j_free <- which(K_vec > 1)

  A <- make_A(Q, M)
  
  m <- rep(1, J)
  m_free <- sample(1:M, length(j_free), TRUE)
  m[j_free] <- m_free
  
  Beta <- make_Beta(Q, m, "High")
  
  p <- stats::rgamma(2^K, 1)
  p <- p/sum(p)
  
  Z <- sample_Z(Y, A, m, Beta, p)
  
  m_save <- matrix(0, niter-nburn, J)
  Beta_save <- vector("list", niter-nburn)
  Z_save <- matrix(0, niter-nburn, N)
  p_save <- matrix(0, niter-nburn, 2^K)
  
  for (iter in 1:niter){
    Omega <- sample_Omega(Z, A, Beta, m)

    Beta <- sample_Beta(Y, Z, A, m, Omega, Beta, Beta_prior, bounds)

    Z <- sample_Z(Y, A, m, Beta, p)

    p <- sample_p(Z, delta)
    
    m_out <- sample_m(Y, Z, A, Beta, Beta_prior, m, bounds, K_vec, pi_m, j_free, M)
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
