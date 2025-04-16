
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace arma;
using namespace std;

// [[Rcpp::export]]
uvec arma_sample(int n, vec prob){
  int k = prob.n_elem;
  uvec out(n, fill::zeros);
  vec comp = regspace(1,  k);
  for(int i = 0; i < n; ++i){
    out(i) = Rcpp::RcppArmadillo::sample(comp, 1, false, prob)[0];
  }
  out = out - 1;
  return out;
}


// [[Rcpp::export]]
arma::rowvec rdirichlet(arma::rowvec alpha){
  int k = alpha.n_elem;
  arma::rowvec y(k);
  for(int i = 0; i < k; i++){
    y[i] = Rcpp::rgamma(1, alpha[i], 1)[0];
  }
  double v = sum(y);
  arma::rowvec x = y/v;
  return x;
}

// [[Rcpp::export]]
Rcpp::List mcmc_cpp( arma::mat data,
                         Rcpp::List w,
                         int K,
                         int burning,
			 arma::mat beta_init,
                         int n_iter = 100000,
                         int save_it = 100){
  
  // Args:
  //
  // data: columns must be 
  //       1. Word 
  //       2. document
  //       3. frequency
  //       The documents column (2) must be in ascending order, 
  //       first with word from doc 1, then from doc 2 so on.
  //
  // w: list of size D (number of documents), each enty being
  //    a bag of words vector in the format 
  //    [word1, word1, word1, word7, word7, word9, ... ]
  //
  // K: number of topics
  
  // Number of words in vocabulary
  int V = data.col(0).max();
  
  // Number of documents
  int D = data.col(1).max();
  
  // Number of words in each document
  NumericVector N(D);
  
  // Define N[[d]] for each document d
  NumericVector lines_document_d;
  int nd = 0;
  int soma = 0;
  int line_i = 0;
  
  for(int d = 0; d < D; d++){
    lines_document_d = find(data.col(1) == d+1);
    nd = lines_document_d.length();
    for(int i = 0; i < nd; i++ ){
      line_i = lines_document_d[i];
      soma = soma + data(line_i,2);
    }
    N(d) = soma;
    soma = 0;
    
  }
  
  // Initializing hyperparameters
  NumericVector alpha(K);
  NumericVector eta(V);
  
  for (int i = 0; i <= K; i++)
  {
    alpha[i] = 1;
    eta[i] = 1;
  }
  
  
  // Initializing parameters
  //arma::mat beta(K,V);

  arma::mat beta = beta_init;
  
  //for (int i = 0; i < K; i++)
  //{
  //  for (int j = 0; j < V; j++)
  //  {
  //    beta(i,j) = 1.0/V;
  //  }
  //}
  
  arma::mat theta(D,K);
  
  for (int i = 0; i < D; i++)
  {
    for (int j = 0; j < K; j++)
    {
      theta(i,j) = 1.0/K;
    }
    
  }
  
  //creating z list
  Rcpp::List z(D);
  for (int d = 0; d < D; d++)
  {
    z[d] = arma::ones(N[d]);
  }
  
  
  //arma::cube beta_chain(K, V, n_iter/save_it - burning);
  arma::cube beta_chain(K, V, n_iter/save_it - burning + 1);
  beta_chain.slice(0) = beta;
  arma::cube theta_chain(D, K, n_iter/save_it - burning);
  
  arma::mat beta_dirichlet(K,V,fill::ones);
  
  for(int iter = 0; iter < n_iter; iter++){
    
    // sample from full conditional distribution of z
    /////////////////////////////////////////////////
    for(int d = 0; d < D; d++){
      arma::vec w_d = w[d];
      arma::vec z_d = z[d];
      for(int n = 0; n < N[d]; n++){
        arma::vec p_zdn(K, fill::ones);
        for(int k = 0; k < K; k++){
          p_zdn[k] = beta(k,w_d[n])*theta(d,k);
        }
        p_zdn = p_zdn/sum(p_zdn);
        z_d[n] = arma_sample(1,p_zdn)[0];
      }
      z[d] = z_d;
    }
    
    //sample from full conditional distribution of beta
    ///////////////////////////////////////////////////
    
    beta_dirichlet = beta_dirichlet*eta[1];
    
    for(int d = 0; d<D; d++){
      
      arma::vec z_d = z[d];
      arma::vec w_d = w[d];
      for(int n = 0; n < N[d]; n++){
        beta_dirichlet(z_d[n], w_d[n] ) = beta_dirichlet(z_d[n], w_d[n] ) + 1;
      }
    }
    
    for(int k = 0; k < K; k++){
      beta.row(k) = rdirichlet(beta_dirichlet.row(k));
    }
    
    //sample from full conditional distribution of theta
    ////////////////////////////////////////////////////
    arma::mat theta_dirichlet(D,K, fill::ones);
    theta_dirichlet = theta_dirichlet*alpha[1];
    for(int d = 0; d<D; d++){
      arma::vec z_d = z[d];
      for(int n = 0; n < N[d]; n++){
        theta_dirichlet(d, z_d[n]) = theta_dirichlet(d, z_d[n])  + 1;
      }
      theta.row(d) = rdirichlet(theta_dirichlet.row(d));
    }
    
    //saving sampled parameters
    if(iter%save_it == 0){
	if(iter/save_it >= burning){
      //beta_chain.slice(iter/save_it - burning) = beta;
      beta_chain.slice(iter/save_it - burning + 1) = beta;
      theta_chain.slice(iter/save_it - burning) = theta;
      Rcout<< "iter " << iter << "\n";
	}
    }
    
  }
  
    
    Rcpp::List lista=Rcpp::List::create(Rcpp::Named("theta") = theta_chain,
                                        Rcpp::Named("beta") = beta_chain);
  return lista;

}
