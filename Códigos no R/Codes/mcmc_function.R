mcmc_lda <- function(data_mat, n_iter = 10, save_it = 10, K = 5
                     ){
  require(gtools)
  
    # Number of words in the vocabulary
  V = max(data_mat[ ,1])
  # number of documents
  D = max(data_mat[, 2])
  # number of words in document d
  N = rep(0, D)
  
  # defining N[d]
  for ( d in 1:D ){
    lines_document_d = which(data_mat$doc == d)
    N[d] = sum( data_mat[lines_document_d, 3] )
  }
  
  # Initializing hyperparameters
  alpha = rep(1, K)
  eta = rep(1, V)
  
  # Initializing parameters
  beta = matrix(1/V, nrow = K, ncol = V)
  theta = matrix(1/K, nrow = D, ncol = K)
  z = rep( list(1), D)
  for (d in 1:D){
    z[[d]] = sample( x=1:K, size = N[d], replace = TRUE)
  }
  
  # We create the w list by disagregating the original matrix of 
  # documents. We pretend that repeated words occur in sequence within
  # each document. This only works because we are dealing with a bag of
  # words model so the order in which words appear are irrelevant.
  w = rep( list(NULL), D)
  for (d in 1:D){
    doc_d = data[which(data$doc == d), c(1,3)]
    for (i in 1:nrow(doc_d) ){
      w[[d]] = c(w[[d]], rep( doc_d$word[i], doc_d$freq[i]) )
    } 
  }
  
  
  beta_chain = array(0, dim = c(K, V, n_iter/save_it) )
  theta_chain = array(0, dim = c(D, K, n_iter/save_it) )
  #z_chain = rep(list(0), n_iter/save_it)
  
  for(iter in 1:n_iter){
    
    # sample from full conditional distribution of beta
    ######################################################
    beta_dirichlet = matrix(eta[1],  nrow = K, ncol = V)
    for (d in 1:D){
      for(n in 1:N[[d]]){
        beta_dirichlet[z[[d]][n], w[[d]][n] ] = beta_dirichlet[z[[d]][n], w[[d]][n] ] + 1
      }
    }
    for(k in 1:K){
      beta[k, ] = rdirichlet(alpha = beta_dirichlet[k, ] )
    }
    
    # sample from full conditional distribution of theta
    ######################################################
    theta_dirichlet = matrix(alpha[1], nrow = D, ncol = K)
    theta = matrix(0, nrow = D, ncol = K)
    for (d in 1:D){
      for (n in 1:N[[d]]){
        theta_dirichlet[ d, z[[d]][n] ] = theta_dirichlet[ d, z[[d]][n] ] + 1
      }
      theta[d, ] = rdirichlet(theta_dirichlet[d, ])
    }
    
    # sample from full conditional distribution of z
    ######################################################
    for(d in 1:D){
      for (n in 1:N[d]){
        p_zdn = rep(0, K)
        for(k in 1:K){
          p_zdn[k] = beta[k, w[[d]][n] ] * theta[d, k]
        }
        p_zdn = p_zdn/sum(p_zdn)
        #DEU ERRO NO SAMPLE
        z[[d]][n] = sample(x=1:K, size = 1, prob = p_zdn)
      }
    }
    
    # saving sampled parameters
    if( iter %% save_it == 0){
      beta_chain[,, iter%/%save_it ] = beta
      theta_chain[,, iter%/%save_it] = theta
      #z_chain[[iter/save_it]] = z  
    }
    # print current iteration
    print(iter)
    
  }
  
  # create mcmc chain with the chains for each parameter
  mcmc_chain = list( "beta" = beta_chain,
                     "theta" = theta_chain)
  
  # returns mcmc chain
  return(mcmc_chain)
  
}
