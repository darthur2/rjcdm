#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppEigen)]]


Eigen::VectorXd sigmoid_cpp(const Eigen::VectorXd& x) {
  Eigen::VectorXd result(x.size());
  for (int i = 0; i < x.size(); ++i) {
    if (x[i] >= 0) {
      double z = std::exp(-x[i]);
      result[i] = 1.0 / (1.0 + z);
    } else {
      double z = std::exp(x[i]);
      result[i] = z / (1.0 + z);
    }
  }
  return result;
}

// [[Rcpp::export]]
Eigen::MatrixXd comp_PZ_cpp(
    const Eigen::MatrixXd& Y,
    const List& A,
    const IntegerVector& m,
    const List& Beta,
    const Eigen::VectorXd& p
) {
  int N = Y.rows();
  int J = Y.cols();
  int CC = p.size();
  
  // -----------------------------
  // 🔵 Pre-extract Beta
  // -----------------------------
  std::vector<Eigen::VectorXd> Beta_cpp(J);
  for (int j = 0; j < J; ++j) {
    Beta_cpp[j] = as<Eigen::VectorXd>(Beta[j]);
  }
  
  // -----------------------------
  // 🔵 Pre-extract A (nested list)
  // -----------------------------
  int M = A.size();
  std::vector<std::vector<Eigen::MatrixXd>> A_cpp(M);
  
  for (int mm = 0; mm < M; ++mm) {
    List A_m = A[mm];
    int Jm = A_m.size();
    
    A_cpp[mm].resize(Jm);
    
    for (int j = 0; j < Jm; ++j) {
      A_cpp[mm][j] = as<Eigen::MatrixXd>(A_m[j]);
    }
  }
  
  // -----------------------------
  // 🔵 Output
  // -----------------------------
  Eigen::MatrixXd PZ(N, CC);
  
  // -----------------------------
  // 🔵 Main loop
  // -----------------------------
  for (int i = 0; i < N; ++i) {
    
    Eigen::VectorXd log_p_i = Eigen::VectorXd::Zero(CC);
    
    for (int j = 0; j < J; ++j) {
      
      double Y_ij = Y(i, j);
      int m_j = m[j] - 1;  // R → C++
      
      const Eigen::VectorXd& beta_j = Beta_cpp[j];
      const Eigen::MatrixXd& A_m_j = A_cpp[m_j][j];
      
      // linear predictor
      Eigen::VectorXd eta = A_m_j * beta_j;
      
      // softmax
      Eigen::VectorXd theta = sigmoid_cpp(eta);
      
      // log-likelihood contribution
      log_p_i.array() += 
        Y_ij * theta.array().log() +
        (1.0 - Y_ij) * (1.0 - theta.array()).log();
    }
    
    // add prior
    log_p_i.array() += p.array().log();
    
    // stabilize
    double max_log = log_p_i.maxCoeff();
    log_p_i.array() -= max_log;
    
    // store
    PZ.row(i) = log_p_i.transpose();
  }
  
  // -----------------------------
  // 🔵 Normalize rows (softmax)
  // -----------------------------
  for (int i = 0; i < N; ++i) {
    Eigen::VectorXd row = PZ.row(i);
    Eigen::VectorXd exp_row = row.array().exp();
    PZ.row(i) = (exp_row / exp_row.sum()).transpose();
  }
  
  return PZ;
}

// [[Rcpp::export]]
Eigen::MatrixXd sample_Z_cpp(
    const Eigen::MatrixXd& Y,
    const List& A,
    const IntegerVector& m,
    const List& Beta,
    const Eigen::VectorXd& p
) {
  
  // -----------------------------
  // 🔵 Step 1: compute PZ
  // -----------------------------
  Eigen::MatrixXd PZ = comp_PZ_cpp(Y, A, m, Beta, p);
  
  int N = PZ.rows();
  int CC = PZ.cols();
  
  // -----------------------------
  // 🔵 Step 2: sample Z
  // -----------------------------
  Eigen::MatrixXd Z = Eigen::MatrixXd::Zero(N, CC);
  
  for (int i = 0; i < N; ++i) {
    
    double u = R::runif(0.0, 1.0);
    double cum = 0.0;
    
    for (int c = 0; c < CC; ++c) {
      cum += PZ(i, c);
      if (u <= cum) {
        Z(i, c) = 1.0;
        break;
      }
    }
  }
  
  return Z;
}