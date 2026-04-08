sample_u_j <- function(beta_j, m_j, m_j_star, K_j){
  # G-DINA: 1
  # ACDM: 2
  # DINA: 3
  # DINO: 4
  
  sigma <- 0.75
  
  if (m_j == 1 & m_j_star == 2){
    
    u_j_star <- numeric(0)
    beta_j_star <- beta_j[1:(K_j+1)]
    u_j <- beta_j[(K_j+2):(2^K_j)]

  } else if (m_j == 1 & m_j_star == 3){
    
    if (K_j == 1){
      u_j_star <- numeric(0)
      beta_j_star <- beta_j
      u_j <- numeric(0)
      
    } else if (K_j == 2){
      u_j_star <- stats::rnorm(1, 4.4, sigma)
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2:(2^K_j)]
      
    } else if (K_j == 3){
      u_j_star <- stats::rnorm(1, 4.4, sigma)
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2:(2^K_j)]
    } else {
      stop("Not a valid value for K_j")
    }
    
  } else if (m_j == 1 & m_j_star == 4){
    
    if (K_j == 1){
      u_j_star <- numeric(0)
      beta_j_star <- beta_j
      u_j <- numeric(0)
      
    } else if (K_j == 2){
      u_j_star <- stats::rnorm(1, 4.4, sigma)
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2:(2^K_j)]
      
    } else if (K_j == 3){
      u_j_star <- stats::rnorm(1, 4.4, sigma)
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2:(2^K_j)]
      
    } else {
      stop("Not a valid value for K_j")
    }

  } else if (m_j == 2 & m_j_star == 3){
    
    if (K_j == 1){
      u_j_star <- numeric(0)
      beta_j_star <- beta_j
      u_j <- numeric(0)
    } else if (K_j == 2){
      u_j_star <- stats::rnorm(1, 4.4, sigma)
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2:(K_j+1)]
      
    } else if (K_j == 3){
      u_j_star <- stats::rnorm(1, 4.4, sigma)
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2:(K_j+1)]
      
    } else {
      stop("Not a valid value for K_j")
    }
    
  } else if (m_j == 2 & m_j_star == 4){
    
    if (K_j == 1){
      u_j_star <- numeric(0)
      beta_j_star <- beta_j
      u_j <- numeric(0)
    } else if (K_j == 2){
      u_j_star <- stats::rnorm(1, 4.4, sigma)
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2:(K_j+1)]
      
    } else if (K_j == 3){
      u_j_star <- stats::rnorm(1, 4.4, sigma)
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2:(K_j+1)]
      
    } else {
      stop("Not a valid value for K_j")
    }
    
  } else if (m_j == 3 & m_j_star == 4){
    
    u_j_star <- 0
    beta_j_star <- beta_j
    u_j <- 0
    
  } else if (m_j == 4 & m_j_star == 3){
    
    u_j_star <- 0
    beta_j_star <- beta_j
    u_j <- 0
    
  } else if (m_j == 4 & m_j_star == 2){
    
    if (K_j == 1){
      u_j_star <- numeric(0)
      beta_j_star <- beta_j
      u_j <- numeric(0)
    } else if (K_j == 2){
      u_j_star <- rtnorm(K_j, 2.2, sigma, 0)
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2]
      
    } else if (K_j == 3){
      u_j_star <- rtnorm(K_j, 1.5, sigma, 0)
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2]
      
    } else {
      stop("Not a valid value for K_j")
    }
    
  } else if (m_j == 4 & m_j_star == 1){
    
    if (K_j == 1){
      u_j_star <- numeric(0)
      beta_j_star <- beta_j
      u_j <- numeric(0)
      
    } else if (K_j == 2){
      u_j_star <- rtnorm(K_j, 1.8, sigma, 0)
      u_j_star <- c(u_j_star, stats::rnorm(1, 0.9, sigma))
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2]
      
    } else if (K_j == 3){
      u_j_star <- rtnorm(K_j, 1.1, sigma, 0)
      u_j_star <- c(u_j_star, stats::rnorm(K_j, 1.1, sigma))
      u_j_star <- c(u_j_star, stats::rnorm(1, -2.2, sigma))
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2]
      
    } else {
      stop("Not a valid value for K_j")
    }
    
  } else if (m_j == 3 & m_j_star == 2){
    
    if (K_j == 1){
      u_j_star <- numeric(0)
      beta_j_star <- beta_j
      u_j <- numeric(0)
    } else if (K_j == 2){
      u_j_star <- rtnorm(K_j, 2.2, sigma, 0)
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2]
      
    } else if (K_j == 3){
      u_j_star <- rtnorm(K_j, 1.5, sigma, 0)
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2]
      
    } else {
      stop("Not a valid value for K_j")
    }
    
  } else if (m_j == 3 & m_j_star == 1){
    
    if (K_j == 1){
      u_j_star <- numeric(0)
      beta_j_star <- beta_j
      u_j <- numeric(0)
      
    } else if (K_j == 2){
      u_j_star <- rtnorm(K_j, 1.8, sigma, 0)
      u_j_star <- c(u_j_star, stats::rnorm(1, 0.9, sigma))
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2]
      
    } else if (K_j == 3){
      u_j_star <- rtnorm(K_j, 1.1, sigma, 0)
      u_j_star <- c(u_j_star, stats::rnorm(K_j, 1.1, sigma))
      u_j_star <- c(u_j_star, stats::rnorm(1, -2.2, sigma))
      beta_j_star <- c(beta_j[1], u_j_star)
      u_j <- beta_j[2]
      
    } else {
      stop("Not a valid value for K_j")
    }
    
  } else if (m_j == 2 & m_j_star == 1){
    
    if (K_j == 1){
      u_j_star <- numeric(0)
      beta_j_star <- beta_j
      u_j <- numeric(0)
      
    } else if (K_j == 2){
      u_j_star <- stats::rnorm(1, 0.9, sigma)
      beta_j_star <- c(beta_j, u_j_star)
      u_j <- numeric(0)
      
    } else if (K_j == 3){
      u_j_star <- stats::rnorm(K_j, 1.1, sigma)
      u_j_star <- c(u_j_star, stats::rnorm(1, -2.2, sigma))
      beta_j_star <- c(beta_j, u_j_star)
      u_j <- numeric(0)
      
    } else {
      stop("Not a valid value for k_j")
    }
    
  } else if (m_j == m_j_star){
    
    u_j_star <- numeric(0)
    beta_j_star <- beta_j
    u_j <- numeric(0)
    
  } else {
    stop("Not a valid model transition")
  }
  
  list(u_j_star = u_j_star,
       beta_j_star = beta_j_star,
       u_j = u_j)
}

comp_ld_u_j <- function(u_j, u_j_star, m_j, m_j_star, K_j){
  sigma <- 0.75
  
  if (m_j == 1 & m_j_star == 2){
    
    if (K_j == 1){
      ld_u_j_star <- 0
      ld_u_j <- 0
      
    } else if (K_j == 2){
      ld_u_j_star <- 0
      ld_u_j <- sum(stats::dnorm(u_j, 0.9, sigma, log = TRUE))
      
    } else if (K_j == 3){
      ld_u_j_star <- 0
      ld_u_j <- sum(stats::dnorm(u_j[1:K_j], 1.1, sigma, log = TRUE)) +
        sum(stats::dnorm(u_j[K_j+1], -2.2, sigma, log = TRUE))
    } else {
      stop("Not a valid value of K_j")
    }
    
  } else if (m_j == 1 & m_j_star == 3){
    
    if (K_j == 1){
      ld_u_j_star <- 0
      ld_u_j <- 0
      
    } else if (K_j == 2){
      ld_u_j_star <- stats::dnorm(u_j_star, 4.4, sigma, log = TRUE)
      ld_u_j <- sum(dtnorm(u_j[1:K_j], 1.8, sigma, 0)) +
        stats::dnorm(u_j[K_j+1], 0.9, sigma, log = TRUE)
      
    } else if (K_j == 3){
      ld_u_j_star <- stats::dnorm(u_j_star, 4.4, sigma, log = TRUE)
      ld_u_j <- sum(dtnorm(u_j[1:K_j], 1.1, sigma, 0)) +
        sum(stats::dnorm(u_j[(K_j+1):(K_j+3)], 1.1, sigma, log = TRUE)) +
        stats::dnorm(u_j[K_j+4], -2.2, sigma, log = TRUE)
      
    } else {
      stop("Not a valid value of K_j")
    }
    
  } else if (m_j == 1 & m_j_star == 4){
    
    if (K_j == 1){
      ld_u_j_star <- 0
      ld_u_j <- 0
      
    } else if (K_j == 2){
      ld_u_j_star <- stats::dnorm(u_j_star, 4.4, sigma, log = TRUE)
      ld_u_j <- sum(dtnorm(u_j[1:K_j], 1.8, sigma, 0)) +
        stats::dnorm(u_j[K_j+1], 0.9, sigma, log = TRUE)
      
    } else if (K_j == 3){
      ld_u_j_star <- stats::dnorm(u_j_star, 4.4, sigma, log = TRUE)
      ld_u_j <- sum(dtnorm(u_j[1:K_j], 1.1, sigma, 0)) +
        sum(stats::dnorm(u_j[(K_j+1):(K_j+3)], 1.1, sigma, log = TRUE)) +
        stats::dnorm(u_j[K_j+4], -2.2, sigma, log = TRUE)
      
    } else {
      stop("Not a valid value of K_j")
    }
    
  } else if (m_j == 2 & m_j_star == 3){
    
    if (K_j == 1){
      ld_u_j_star <- 0
      ld_u_j <- 0
      
    } else if (K_j == 2){
      ld_u_j_star <- stats::dnorm(u_j_star, 4.4, sigma, log = TRUE)
      ld_u_j <- sum(dtnorm(u_j, 2.2, sigma, 0))
      
    } else if (K_j == 3){
      ld_u_j_star <- stats::dnorm(u_j_star, 4.4, sigma, log = TRUE)
      ld_u_j <- sum(dtnorm(u_j, 1.5, sigma, 0))
      
    } else {
      stop("Not a valid value of K_j")
    }
    
  } else if (m_j == 2 & m_j_star == 4){
    
    if (K_j == 1){
      ld_u_j_star <- 0
      ld_u_j <- 0
      
    } else if (K_j == 2){
      ld_u_j_star <- stats::dnorm(u_j_star, 4.4, sigma, log = TRUE)
      ld_u_j <- sum(dtnorm(u_j, 2.2, sigma, 0))
      
    } else if (K_j == 3){
      ld_u_j_star <- stats::dnorm(u_j_star, 4.4, sigma, log = TRUE)
      ld_u_j <- sum(dtnorm(u_j, 1.5, sigma, 0))
      
    } else {
      stop("Not a valid value of K_j")
    }
    
  } else if (m_j == 3 & m_j_star == 4){
    
    ld_u_j_star <- 0
    ld_u_j <- 0
    
  } else if (m_j == 4 & m_j_star == 3){
    
    ld_u_j_star <- 0
    ld_u_j <- 0
    
  } else if (m_j == 4 & m_j_star == 2){
    if (K_j == 1){
      ld_u_j_star <- 0
      ld_u_j <- 0
      
    } else if (K_j == 2){
      ld_u_j_star <- sum(dtnorm(u_j_star, 2.2, sigma, 0))
      ld_u_j <- stats::dnorm(u_j, 4.4, sigma, log = TRUE)

    } else if (K_j == 3){
      ld_u_j_star <- sum(dtnorm(u_j_star, 1.5, sigma, 0))
      ld_u_j <- stats::dnorm(u_j, 4.4, sigma, log = TRUE)
      
    } else {
      stop("Not a valid value of K_j")
    }

  } else if (m_j == 4 & m_j_star == 1){
    
    if (K_j == 1){
      ld_u_j_star <- 0
      ld_u_j <- 0
      
    } else if (K_j == 2){
      ld_u_j_star <- sum(dtnorm(u_j_star[1:K_j], 1.8, sigma, 0)) +
        stats::dnorm(u_j_star[K_j+1], 0.9, sigma, log = TRUE)
      ld_u_j <- stats::dnorm(u_j, 4.4, sigma, log = TRUE)
      
    } else if (K_j == 3){
      ld_u_j_star <- sum(dtnorm(u_j_star[1:K_j], 1.1, sigma, 0)) +
        sum(stats::dnorm(u_j_star[(K_j+1):(K_j+3)], 1.1, sigma, log = TRUE)) +
        stats::dnorm(u_j_star[K_j+4], -2.2, sigma, log = TRUE)
      ld_u_j <- stats::dnorm(u_j, 4.4, sigma, log = TRUE)
      
    } else {
      stop("Not a valid value of K_j")
    }
    
  } else if (m_j == 3 & m_j_star == 2){

    if (K_j == 1){
      ld_u_j_star <- 0
      ld_u_j <- 0
      
    } else if (K_j == 2){
      ld_u_j_star <- sum(dtnorm(u_j_star, 2.2, sigma, 0))
      ld_u_j <- stats::dnorm(u_j, 4.4, sigma, log = TRUE)

    } else if (K_j == 3){
      ld_u_j_star <- sum(dtnorm(u_j_star, 1.5, sigma, 0))
      ld_u_j <- stats::dnorm(u_j, 4.4, sigma, log = TRUE)
      
    } else {
      stop("Not a valid value of K_j")
    }

  } else if (m_j == 3 & m_j_star == 1){
    
    if (K_j == 1){
      ld_u_j_star <- 0
      ld_u_j <- 0
      
    } else if (K_j == 2){
      ld_u_j_star <- sum(dtnorm(u_j_star[1:K_j], 1.8, sigma, 0)) +
        stats::dnorm(u_j_star[K_j+1], 0.9, sigma, log = TRUE)
      ld_u_j <- stats::dnorm(u_j, 4.4, sigma, log = TRUE)
      
    } else if (K_j == 3){
      ld_u_j_star <- sum(dtnorm(u_j_star[1:K_j], 1.1, sigma, 0)) +
        sum(stats::dnorm(u_j_star[(K_j+1):(K_j+3)], 1.1, sigma, log = TRUE)) +
        stats::dnorm(u_j_star[K_j+4], -2.2, sigma, log = TRUE)
      ld_u_j <- stats::dnorm(u_j, 4.4, sigma, log = TRUE)
      
    } else {
      stop("Not a valid value of K_j")
    } 
    
  } else if (m_j == 2 & m_j_star == 1){
    
    if (K_j == 1){
      ld_u_j_star <- 0
      ld_u_j <- 0
      
    } else if (K_j == 2){
      ld_u_j_star <- sum(stats::dnorm(u_j_star, 0.9, sigma, log = TRUE))
      ld_u_j <- 0

    } else if (K_j == 3){
      ld_u_j_star <- sum(stats::dnorm(u_j_star[1:K_j], 1.1, sigma, log = TRUE)) +
        sum(stats::dnorm(u_j_star[K_j+1], -2.2, sigma, log = TRUE))
      ld_u_j <- 0
    } else {
      stop("Not a valid value of K_j")
    }
    
  } else if (m_j == m_j_star){
    
    ld_u_j_star <- 0
    ld_u_j <- 0
    
  } else {
    stop("Not a valid model transition")
  }
  
  list(ld_u_j_star = ld_u_j_star,
       ld_u_j = ld_u_j)
}

comp_ll_m_j <- function(Y_j, Z, A_m_j, beta_j){
  lin_pred_j <- Z%*%(A_m_j%*%beta_j)
  theta_j <- sigmoid(lin_pred_j)
  
  theta_j[theta_j < 1e-06] <- 1e-06
  theta_j[theta_j > 1-1e-06] <- 1-1e-06
  
  ll_m_j <- sum(Y_j*log(theta_j) + (1-Y_j)*log(1-theta_j))
  
  ll_m_j
}

comp_lp_m_j <- function(Y_j, Z, 
                        A_m_j, 
                        beta_j,
                        mu_m_j,
                        sigma_m_j, 
                        pi_m_j, 
                        m_j, 
                        lower_m_j,
                        upper_m_j,
                        K_j){
  ll_m_j <- comp_ll_m_j(Y_j, Z, A_m_j, beta_j)
  lp_beta_j <- comp_lp_beta_j(beta_j, m_j, mu_m_j, sigma_m_j, lower_m_j, upper_m_j, K_j)
  
  lp_m_j <- log(pi_m_j) + lp_beta_j + ll_m_j
  
  lp_m_j
}

comp_lp_beta_j <- function(beta_j, 
                           m_j, 
                           mu_m_j, 
                           sigma_m_j, 
                           lower_m_j, 
                           upper_m_j, 
                           K_j){
  
  if (m_j == 1){
    lp_beta_j <- stats::dnorm(beta_j[1], 
                              mu_m_j[1], 
                              sigma_m_j[1], log = TRUE) +
      sum(dtnorm(beta_j[2:(K_j+1)], 
                 mu_m_j[2:(K_j+1)], 
                 sigma_m_j[2:(K_j+1)], 
                 lower_m_j[2:(K_j+1)],
                 upper_m_j[2:(K_j+1)])) +
      sum(stats::dnorm(beta_j[(K_j+2):(2^K_j)], 
                       mu_m_j[(K_j+2):(2^K_j)], 
                       sigma_m_j[(K_j+2):(2^K_j)], log = TRUE))
    
  } else if (m_j == 2){
    lp_beta_j <- stats::dnorm(beta_j[1], 
                              mu_m_j[1], 
                              sigma_m_j[1], log = TRUE) +
      sum(dtnorm(beta_j[2:(K_j+1)], 
                 mu_m_j[2:(K_j+1)],
                 sigma_m_j[2:(K_j+1)],
                 lower_m_j[2:(K_j+1)],
                 upper_m_j[2:(K_j+1)]))
    
  } else if (m_j == 3){
    lp_beta_j <- stats::dnorm(beta_j[1], 
                              mu_m_j[1], 
                              sigma_m_j[1], log = TRUE) +
      dtnorm(beta_j[2], 
             mu_m_j[2], 
             sigma_m_j[2], 
             lower_m_j[2],
             upper_m_j[2])
    
  } else if (m_j == 4){
    lp_beta_j <- stats::dnorm(beta_j[1], 
                              mu_m_j[1], 
                              sigma_m_j[1], log = TRUE) +
      dtnorm(beta_j[2], 
             mu_m_j[2], 
             sigma_m_j[2], 
             lower_m_j[2],
             upper_m_j[2])
    
  } else {
    stop("Not a valid model")
  }
  
  lp_beta_j
}

sigmoid <- function(x){
  exp(x)/(1+exp(x))
}

comp_PZ <- function(Y, A, m, Beta, p){
  PZ <- comp_PZ_cpp(Y, A, m, Beta, p)
  
  PZ
}

rtnorm <- function(n, mu, sigma, lower = -Inf, upper = Inf){
  n_draw <- 0
  draw_count <- 1
  x <- numeric(draw_count)
  
  while (n_draw < n){
    draw <- stats::rnorm(1, mu, sigma)
    
    if ((draw > lower) & (draw < upper)){
      x[draw_count] <- draw
      draw_count <- draw_count + 1
      n_draw <- n_draw + 1
    }
  }
  
  x
}

dtnorm <- function(x, mu, sigma, lower = -Inf, upper = Inf, log = TRUE){
  z <- (x-mu)/sigma
  z_lower <- (lower-mu)/sigma
  z_upper <- (upper-mu)/sigma
  
  Z <- stats::pnorm(z_upper) - stats::pnorm(z_lower)
  
  if (log){
    value <- -1/2*log(2*pi) - 1/2*z^2 - log(Z) - log(sigma)
    value[(x < lower) | (x > upper)] <- -Inf
  } else {
    value <- 1/sqrt(2*pi)*exp(-1/2*z^2)/(sigma*Z)
    value[(x < lower) | (x > upper)] <- 0
  }
  
  value
}

#' Initialize Item Parameter Values for CDM Models
#'
#' Generates initial values for item parameters (\eqn{\beta}) based on the
#' Q-matrix, model indicators, and a specified item quality level. The function
#' returns a list of parameter vectors, one for each item, with structure
#' determined by the selected cognitive diagnosis model (CDM).
#'
#' @details
#' For each item \eqn{j}, the number of required attributes \eqn{K_j} is computed
#' from the Q-matrix. The parameter vector \eqn{\boldsymbol{\beta}_j} is then
#' initialized according to:
#' \itemize{
#'   \item The selected model \eqn{m_j} (e.g., G-DINA, ACDM, DINA, DINO)
#'   \item The number of attributes \eqn{K_j}
#'   \item The item quality level specified by \code{IQ}
#' }
#'
#' Two predefined quality levels are supported:
#' \itemize{
#'   \item \code{"High"}: Strong item discrimination (larger parameter magnitudes)
#'   \item \code{"Low"}: Weaker item discrimination (smaller parameter magnitudes)
#' }
#'
#' The structure of \eqn{\boldsymbol{\beta}_j} depends on the model:
#' \itemize{
#'   \item Model 1 (G-DINA): Includes main and interaction effects (length \eqn{2^{K_j}})
#'   \item Model 2 (ACDM): Includes intercept and main effects (length \eqn{K_j + 1})
#'   \item Models 3 & 4 (DINA/DINO): Include intercept and a single effect (length 2)
#' }
#'
#' Currently, only items with up to three attributes (\eqn{K_j \le 3}) are supported.
#'
#' @param Q A binary \eqn{K \times J} Q-matrix indicating which attributes
#'   are required by each item.
#'
#' @param m An integer vector of length \eqn{J} specifying the model index
#'   for each item.
#'
#' @param IQ A character string specifying item quality level. Must be one of:
#'   \code{"High"}, \code{"Low"}, or \code{"Random"} (not yet implemented).
#'
#' @return A list of length \eqn{J}, where each element is a numeric vector
#'   containing the initialized parameter values for the corresponding item.
#'
#' @section Models:
#' The following model indices are used:
#' \itemize{
#'   \item 1: G-DINA (general model with interactions)
#'   \item 2: ACDM (additive model)
#'   \item 3: DINA (conjunctive model)
#'   \item 4: DINO (disjunctive model)
#' }
#'
#' @examples
#' \dontrun{
#' Q <- matrix(c(1,0,1,
#'               0,1,1), nrow = 2)
#'
#' m <- c(1, 2, 3)
#'
#' Beta <- make_Beta(Q, m, IQ = "High")
#'
#' Beta[[1]]  # parameters for item 1
#' }
#'
#' @export
make_Beta <- function(Q, m, IQ){
  K_vec <- rowSums(Q)
  J <- length(K_vec)
  Beta <- vector("list", J)
  
  for (j in 1:J){
    m_j <- m[j]
    K_j <- K_vec[j]
    
    if (IQ == "High"){
      if (m_j == 1){
        if (K_j == 1){
          Beta[[j]] <- c(-2.1972, 4.3944)
        } else if (K_j == 2){
          Beta[[j]] <- c(-2.1972, 1.7918, 1.7918, 0.8831)
        } else if (K_j == 3){
          Beta[[j]] <- c(-2.1972, 1.0986, 1.0986, 1.0986, 1.0986, 1.0986, 1.0986, -2.1972)
        } else {
          stop("Cannot have K_j > 3 currently")
        }
        
      } else if (m_j == 2){
        if (K_j == 1){
          Beta[[j]] <- c(-2.1972, 4.3944)
        } else if (K_j == 2){
          Beta[[j]] <- c(-2.1972, 2.1972, 2.1972)
        } else if (K_j == 3){
          Beta[[j]] <- c(-2.1972, 1.4648, 1.4648, 1.4648)
        } else {
          stop("Cannot have K_j > 3 currently")
        }
        
      } else if (m_j == 3){
        Beta[[j]] <- c(-2.1972, 4.3944)
      } else if (m_j == 4){
        Beta[[j]] <- c(-2.1972, 4.3944)
      } else {
        stop("Not a valid model.")
      }
    } else if (IQ == "Low"){
      if (m_j == 1){
        if (K_j == 1){
          Beta[[j]] <- c(-0.8473, 1.6946)
        } else if (K_j == 2){
          Beta[[j]] <- c(-0.8473, 0.4418, 0.4418, 0.8109)
        } else if (K_j == 3){
          Beta[[j]] <- c(-0.8473, 0.4418, 0.4418, 0.4418, 0.3691, 0.3691, 0.3691, -0.7376)
        } else {
          stop("Cannot have K_j > 3 currently")
        }
        
      } else if (m_j == 2){
        if (K_j == 1){
          Beta[[j]] <- c(-0.8473, 1.6946)
        } else if (K_j == 2){
          Beta[[j]] <- c(-0.8473, 0.8473, 0.8473)
        } else if (K_j == 3){
          Beta[[j]] <- c(-0.8473, 0.5649, 0.5649, 0.5649)
        } else {
          stop("Cannot have K_j > 3 currently")
        }
        
      } else if (m_j == 3){
        Beta[[j]] <- c(-0.8473, 1.6946)
      } else if (m_j == 4){
        Beta[[j]] <- c(-0.8473, 1.6946)
      } else {
        stop("Not a valid model.")
      }
    } else if (IQ == "Random"){
      
    } else {
      stop("Not a valid value for IQ.")
    }
  }
  
  Beta
}

make_A <- function(Q, M = 4){
  A <- vector("list", M)
  K <- ncol(Q)
  patterns <- make_patterns(K)
  
  for (m in 1:M){
    A[[m]] <- make_A_m(Q, patterns, m)
  }
  
  A
}

make_A_m <- function(Q, patterns, m){
  J <- nrow(Q)
  
  A_m <- vector("list", J)
  
  for (j in 1:J){
    A_m[[j]] <- make_A_m_j(Q, patterns, m, j)
  }
  
  A_m
}

make_A_m_j <- function(Q, patterns, m, j){
  K <- ncol(Q)
  CC <- 2^K
  q_j <- Q[j,]
  K_j <- sum(q_j)

  if (m == 1){
    A_m_j <- matrix(0, 2^K, 2^K_j)
  } else if (m == 2){
    A_m_j <- matrix(0, CC, K_j+1)
  } else if (m %in% c(3, 4)){
    A_m_j <- matrix(0, CC, 2)
  } else {
    stop("Not a valid model construction")
  }
  
  for (cc in 1:CC){
    alpha_cc <- patterns[cc,]
    A_m_j[cc,] <- make_A_m_j_cc(q_j, alpha_cc, m)
  }
  
  A_m_j
}

make_A_m_j_cc <- function(q_j, alpha_cc, m){
  K_j <- sum(q_j)
  
  if (m == 1){
    A_m_j_cc <- gdina_design_vector(q_j, alpha_cc)
  } else if (m == 2){
    A_m_j_cc <- acdm_design_vector(q_j, alpha_cc)
  } else if (m == 3){
    A_m_j_cc <- dina_design_vector(q_j, alpha_cc)
  } else if (m == 4){
    A_m_j_cc <- dino_design_vector(q_j, alpha_cc)
  }
  else {
    stop("Not a valid model.")
  }
  
  A_m_j_cc
}

gdina_design_vector <- function(q_j, alpha_c) {
  # Indices of required attributes
  req_idx <- which(q_j == 1)
  
  # Number of required attributes
  K <- length(req_idx)
  
  # Initialize design vector with intercept
  design_vec <- c(1)
  
  # If no required attributes, return just intercept
  if (K == 0) return(design_vec)
  
  # Extract relevant alpha values
  alpha_req <- alpha_c[req_idx]
  
  # Loop over interaction orders (1-way up to K-way)
  for (k in 1:K) {
    combos <- utils::combn(K, k)
    
    # If only one combination, ensure it's a matrix
    if (k == 1) combos <- matrix(combos, nrow = 1)
    
    # Compute product for each combination
    for (col in 1:ncol(combos)) {
      design_vec <- c(design_vec, prod(alpha_req[combos[, col]]))
    }
  }
  
  design_vec
}

acdm_design_vector <- function(q_j, alpha_c) {
  # Indices of required attributes
  req_idx <- which(q_j == 1)
  
  # Start with intercept
  design_vec <- c(1)
  
  # Add main effects (only required attributes)
  if (length(req_idx) > 0) {
    design_vec <- c(design_vec, alpha_c[req_idx])
  }
  
  design_vec
}

dina_design_vector <- function(q_j, alpha_c) {
  # Compute eta (AND gate)
  eta <- prod(alpha_c[q_j == 1])
  
  # Design vector: intercept + eta
  design_vec <- c(1, eta)
  
  design_vec
}

dino_design_vector <- function(q_j, alpha_c) {
  # Extract required attributes
  alpha_req <- alpha_c[q_j == 1]
  
  # OR gate: 1 if any required attribute is mastered
  eta <- as.numeric(any(alpha_req == 1))
  
  # Design vector
  design_vec <- c(1, eta)
  
  design_vec
}

#' Generate All Binary Attribute Patterns
#'
#' Generates all possible binary attribute mastery patterns of length \eqn{K}.
#' Each pattern represents a unique combination of mastered (1) and
#' non-mastered (0) attributes.
#'
#' @details
#' The function enumerates all \eqn{2^K} binary vectors of length \eqn{K},
#' corresponding to the full latent class space in cognitive diagnosis models.
#' Patterns are constructed by iterating over all possible numbers of mastered
#' attributes (from 0 to \eqn{K}) and generating combinations of attribute indices.
#'
#' The resulting matrix has:
#' \itemize{
#'   \item \eqn{2^K} rows (all possible patterns)
#'   \item \eqn{K} columns (attributes)
#' }
#'
#' @param K Integer specifying the number of attributes.
#'
#' @return A binary matrix of dimension \eqn{2^K \times K}, where each row
#'   represents a unique attribute pattern.
#'
#' @examples
#' make_patterns(2)
#' #      [,1] [,2]
#' # [1,]    0    0
#' # [2,]    1    0
#' # [3,]    0    1
#' # [4,]    1    1
#'
#' make_patterns(3)
#' # 8 possible patterns
#'
#' @export
make_patterns <- function(K) {
  out <- list()
  
  for (m in 0:K) {
    combs <- utils::combn(K, m, simplify = FALSE)
    for (c in combs) {
      row <- integer(K)
      row[c] <- 1
      out[[length(out) + 1]] <- row
    }
  }
  
  patterns <- do.call(rbind, out)
  
  patterns
}

#' Simulate Data from an RJCDM Model
#'
#' Generates synthetic response data from a cognitive diagnosis model (CDM)
#' under a specified set of parameters. The function simulates latent class
#' memberships, item responses, and returns the full data-generating structure.
#'
#' @details
#' The function simulates data according to the RJCDM framework:
#'
#' \enumerate{
#'   \item Latent class memberships \eqn{Z} are sampled from a multinomial
#'   distribution with probabilities \eqn{\mathbf{p}}.
#'   \item For each item \eqn{j}, response probabilities are computed as:
#'   \deqn{
#'   \theta_{ij} =
#'   \frac{\exp\left( Z_i A_{m_j, j} \beta_j \right)}
#'   {1 + \exp\left( Z_i A_{m_j, j} \beta_j \right)}
#'   }
#'   \item Responses are then sampled:
#'   \deqn{
#'   y_{ij} \sim \text{Bernoulli}(\theta_{ij})
#'   }
#' }
#'
#' The design matrix \eqn{A} is constructed using \code{make_A()}, and the
#' logistic transformation is applied via \code{sigmoid()}.
#'
#' @param N Integer specifying the number of individuals.
#'
#' @param Q A binary \eqn{K \times J} Q-matrix indicating attribute requirements
#'   for each item.
#'
#' @param m An integer vector of length \eqn{J} specifying the model index
#'   for each item.
#'
#' @param Beta A list of length \eqn{J}, where each element is a numeric vector
#'   of item parameters corresponding to the model specified in \code{m}.
#'
#' @param p A vector of length \eqn{2^K} giving latent class probabilities.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{Y}: Simulated binary response matrix (\eqn{N \times J})
#'   \item \code{Q}: The input Q-matrix
#'   \item \code{Z}: Simulated latent class indicator matrix (\eqn{N \times 2^K})
#'   \item \code{params}: A list containing the true generating parameters:
#'   \itemize{
#'     \item \code{Beta}: Item parameters
#'     \item \code{p}: Latent class probabilities
#'     \item \code{m}: Model indicators
#'   }
#' }
#'
#' @section Model:
#' The function supports multiple CDM specifications through \code{m}:
#' \itemize{
#'   \item 1: G-DINA
#'   \item 2: ACDM
#'   \item 3: DINA
#'   \item 4: DINO
#' }
#'
#' @examples
#' \dontrun{
#' Q <- matrix(c(1,0,1,
#'               0,1,1), nrow = 2)
#'
#' K <- ncol(Q)
#' J <- nrow(Q)
#'
#' m <- rep(1, J)
#' Beta <- make_Beta(Q, m, "High")
#' p <- rep(1/(2^K), 2^K)
#'
#' sim <- make_rjcdm_data(N = 500, Q = Q, m = m, Beta = Beta, p = p)
#'
#' sim$Y
#' sim$Z
#' }
#' @export
make_rjcdm_data <- function(N, Q, m, Beta, p){
  K <- ncol(Q)
  J <- nrow(Q)
  
  CC <- 2^K
  I_Z <- diag(CC)
  
  Y <- matrix(0, N, J)
  
  z_cat <- sample(1:CC, N, TRUE, prob = p)
  Z <- I_Z[z_cat,]
  
  A <- make_A(Q)
  
  for (j in 1:J){
    p_j <- sigmoid(Z%*%(A[[m[j]]][[j]]%*%Beta[[j]]))
    Y[,j] <- stats::rbinom(N, 1, p_j)
  }
  
  rjcdm_data <- list(Y = Y,
                     Q = Q,
                     Z = Z,
                     params = list(Beta = Beta,
                                   p = p,
                                   m = m))
  
  rjcdm_data
}

#' Generate Simulated RJCDM Data with Random Model Assignment
#'
#' Generates synthetic response data from a cognitive diagnosis model (CDM)
#' under randomly assigned item models and predefined item quality levels.
#' This function is a convenience wrapper around \code{make_rjcdm_data()} for
#' simulation studies.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Assigns a model \eqn{m_j} to each item:
#'   \itemize{
#'     \item Items requiring only one attribute are fixed to model 1 (G-DINA)
#'     \item Items requiring multiple attributes are randomly assigned to one
#'     of \eqn{M} models
#'   }
#'   \item Generates item parameters \eqn{\beta_j} using \code{make_Beta()}
#'   based on the specified item quality level \code{IQ}
#'   \item Sets latent class probabilities to a uniform distribution:
#'   \deqn{
#'   p_c = \frac{1}{2^K}, \quad c = 1, \ldots, 2^K
#'   }
#'   \item Simulates response data using \code{make_rjcdm_data()}
#' }
#'
#' This function is useful for benchmarking and simulation studies where
#' model heterogeneity across items is desired.
#'
#' @param N Integer specifying the number of individuals.
#'
#' @param Q A binary \eqn{K \times J} Q-matrix indicating attribute requirements
#'   for each item.
#'
#' @param IQ Character string specifying item quality level. Must be one of:
#'   \code{"High"} or \code{"Low"}.
#'
#' @param M Integer specifying the number of candidate CDM models (default = 4).
#'
#' @return A list as returned by \code{make_rjcdm_data()}, containing:
#' \itemize{
#'   \item \code{Y}: Simulated binary response matrix (\eqn{N \times J})
#'   \item \code{Q}: The Q-matrix
#'   \item \code{Z}: Latent class indicator matrix
#'   \item \code{params}: A list with generating parameters:
#'   \itemize{
#'     \item \code{Beta}: Item parameters
#'     \item \code{p}: Latent class probabilities
#'     \item \code{m}: Model indicators
#'   }
#' }
#'
#' @section Model Space:
#' The following model indices are used:
#' \itemize{
#'   \item 1: G-DINA
#'   \item 2: ACDM
#'   \item 3: DINA
#'   \item 4: DINO
#' }
#'
#' @examples
#' \dontrun{
#' Q <- matrix(c(1,0,1,
#'               0,1,1), nrow = 2)
#'
#' sim <- make_rjcdm_sim_data(
#'   N = 500,
#'   Q = Q,
#'   IQ = "High"
#' )
#'
#' sim$Y
#' sim$params$m  # true item models
#' }
#' @export
make_rjcdm_sim_data <- function(N, Q, IQ, M = 4){
  J <- nrow(Q)
  K <- ncol(Q)
  K_vec <- rowSums(Q)
  j_free <- which(K_vec > 1)
  m <- rep(1, J)
  m[j_free] <- sample(1:M, length(j_free), TRUE)
  
  Beta <- make_Beta(Q, m, IQ)
  
  p <- rep(1/2^K, 2^K)
  
  sim_data <- make_rjcdm_data(N, Q, m, Beta, p)
  
  sim_data
}

#' Generate Parameter Bounds for RJCDM Models
#'
#' Constructs truncation bounds for item parameters (\eqn{\beta}) across
#' multiple cognitive diagnosis models. The bounds are used to define
#' truncated normal priors for item parameters during MCMC sampling.
#'
#' @details
#' For each model \eqn{m = 1, \ldots, M}, the function generates parameter
#' bounds for all items based on the number of required attributes
#' \eqn{K_j}. The bounds are created by calling \code{make_bounds_m()},
#' which returns model-specific lower and upper limits for each item.
#'
#' The output is a nested list structure:
#' \itemize{
#'   \item Level 1: Model index \eqn{m}
#'   \item Level 2: Item index \eqn{j}
#'   \item Each element contains:
#'   \itemize{
#'     \item \code{lower_j}: Vector of lower bounds for \eqn{\beta_j}
#'     \item \code{upper_j}: Vector of upper bounds for \eqn{\beta_j}
#'   }
#' }
#'
#' These bounds are used in truncated normal sampling steps for item
#' parameters within the RJMCMC algorithm.
#'
#' @param Q A binary \eqn{K \times J} Q-matrix indicating attribute
#'   requirements for each item.
#'
#' @param M Integer specifying the number of candidate CDM models
#'   (default = 4).
#'
#' @return A list of length \eqn{M}, where each element is itself a list
#'   of length \eqn{J} containing lower and upper bounds for each item's
#'   parameter vector.
#'
#'
#' @examples
#' \dontrun{
#' Q <- matrix(c(1,0,1,
#'               0,1,1), nrow = 2)
#'
#' bounds <- make_bounds(Q)
#'
#' bounds[[1]][[1]]$lower_j  # lower bounds for item 1 under model 1
#' bounds[[1]][[1]]$upper_j  # upper bounds for item 1 under model 1
#' }
#' @export
make_bounds <- function(Q, M = 4){
  bounds <- vector("list", M)
  K_vec <- rowSums(Q)
  
  for (m in 1:M){
    bounds[[m]] <- make_bounds_m(K_vec, m)
  }
  
  bounds
}

make_bounds_m <- function(K_vec, m){
  J <- length(K_vec)
  
  bounds_m <- vector("list", J)
  
  for (j in 1:J){
    K_j <- K_vec[j]
    bounds_m[[j]] <- make_bounds_m_j(K_j, m)
  }
  
  bounds_m
}

make_bounds_m_j <- function(K_j, m){
  
  if (m == 1){
    lower_j <- c(-Inf, rep(0, K_j), rep(-Inf, 2^K_j-K_j-1))
    upper_j <- rep(Inf, 2^K_j)
      
  } else if (m == 2){
    lower_j <- c(-Inf, rep(0, K_j))
    upper_j <- rep(Inf, K_j + 1)
    
  } else if (m == 3){
    lower_j <- c(-Inf, 0)
    upper_j <- rep(Inf, 2)
    
  } else if (m == 4){
    lower_j <- c(-Inf, 0)
    upper_j <- rep(Inf, 2)
    
  } else {
    stop("Not a valid model")
  }
  
  list(lower_j = lower_j,
       upper_j = upper_j)
}

#' Generate Prior Specifications for Item Parameters
#'
#' Constructs prior distributions for item parameters (\eqn{\beta}) across
#' multiple cognitive diagnosis models. The priors are used in the Bayesian
#' estimation procedure for RJCDM.
#'
#' @details
#' For each model \eqn{m = 1, \ldots, M}, the function generates prior
#' specifications for all items based on the number of required attributes
#' \eqn{K_j}. The priors are created by calling \code{make_Beta_prior_m()},
#' which returns model-specific mean and standard deviation parameters
#' for each item.
#'
#' The output is a nested list structure:
#' \itemize{
#'   \item Level 1: Model index \eqn{m}
#'   \item Level 2: Item index \eqn{j}
#'   \item Each element contains:
#'   \itemize{
#'     \item \code{mu_m_j}: Vector of prior means for \eqn{\beta_j}
#'     \item \code{sigma_m_j}: Vector of prior standard deviations for \eqn{\beta_j}
#'   }
#' }
#'
#' These priors are used in truncated normal sampling of item parameters
#' within the RJMCMC algorithm.
#'
#' @param Q A binary \eqn{K \times J} Q-matrix indicating attribute
#'   requirements for each item.
#'
#' @param M Integer specifying the number of candidate CDM models
#'   (default = 4).
#'
#' @return A list of length \eqn{M}, where each element is itself a list
#'   of length \eqn{J} containing prior mean and standard deviation
#'   vectors for each item's parameter vector.
#'
#'
#' @examples
#' \dontrun{
#' Q <- matrix(c(1,0,1,
#'               0,1,1), nrow = 2)
#'
#' Beta_prior <- make_Beta_prior(Q)
#'
#' Beta_prior[[1]][[1]]$mu_m_j      # prior means for item 1 under model 1
#' Beta_prior[[1]][[1]]$sigma_m_j   # prior SDs for item 1 under model 1
#' }
#' @export
make_Beta_prior <- function(Q, M = 4){
  Beta_prior <- vector("list", M)
  K_vec <- rowSums(Q)
  
  for (m in 1:M){
    Beta_prior[[m]] <- make_Beta_prior_m(K_vec, m)
  }
  
  Beta_prior
}

make_Beta_prior_m <- function(K_vec, m){
  J <- length(K_vec)
  prior_m <- vector("list", J)
  
  for (j in 1:J){
    K_j <- K_vec[j]
    prior_m[[j]] <- make_Beta_prior_m_j(K_j, m)
  }
  
  prior_m
}

make_Beta_prior_m_j <- function(K_j, m){
  sigma <- 0.75
  
  if (m == 1){
    if (K_j == 1){
      mu_m_j <- c(-2.2, 4.4)
      sigma_m_j <- c(sigma, sigma)
    } else if (K_j == 2){
      mu_m_j <- c(-2.2, 1.8, 1.8, 0.9)
      sigma_m_j <- c(sigma, sigma, sigma, sigma)
    } else if (K_j == 3){
      mu_m_j <- c(-2.2, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, -2.2)
      sigma_m_j <- c(sigma, sigma, sigma, sigma, sigma, sigma, sigma, sigma)
    } else {
      stop("Not a valid value for K_j")
    }
  } else if (m == 2){
    if (K_j == 1){
      mu_m_j <- c(-2.2, 4.4)
      sigma_m_j <- c(sigma, sigma)
    } else if (K_j == 2){
      mu_m_j <- c(-2.2, 2.2, 2.2)
      sigma_m_j <- c(sigma, sigma, sigma)
    } else if (K_j == 3){
      mu_m_j <- c(-2.2, 1.5, 1.5, 1.5)
      sigma_m_j <- c(sigma, sigma, sigma, sigma)
    } else {
      stop("Not a valid value for K_j")
    }
  } else if (m == 3){
    mu_m_j <- c(-2.2, 4.4)
    sigma_m_j <- c(sigma, sigma)
  } else if (m == 4){
    mu_m_j <- c(-2.2, 4.4)
    sigma_m_j <- c(sigma, sigma)
  } else {
    stop("Not a valid model")
  }
  
  list(mu_m_j = mu_m_j,
       sigma_m_j = sigma_m_j)
}