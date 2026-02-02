data{
  int n;
  vector[n] t;
  vector[n] m;
  array[n] int replicate;
  int n_replicate;
}

parameters{
  // Parameters describing mean
  vector[n_replicate] alpha;
  vector<lower=0>[n_replicate] mu;
  vector<lower=0>[n_replicate] tau;
  
  // Likelihood standard deviation
  real<lower=0> sigma;
}

model{
  // Priors
  alpha ~ normal( 0 , 0.02 );
  mu ~ gamma( square(60) / square(40) , 60 / square(40) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  sigma ~ exponential( 1 );
  
  // Model (constant rate parameterisation)
  vector[n] m_mu = exp(
      t .* alpha[replicate] - 
      ( alpha[replicate] + tau[replicate] ) .* (
        log1p_exp( t - mu[replicate] ) - 
        log1p_exp( -mu[replicate] )
      )
  );

  // Normal likelihood
  m ~ normal( m_mu , sigma );
}

generated quantities{
  // Save mean
  vector[n] m_mu = exp(
      t .* alpha[replicate] - 
      ( alpha[replicate] + tau[replicate] ) .* (
        log1p_exp( t - mu[replicate] ) - 
        log1p_exp( -mu[replicate] )
      )
  );
  
  // Save pointwise log-likelihood
  vector[n] log_lik;
  for(i in 1:n) log_lik[i] = normal_lpdf( m[i] | m_mu[i] , sigma );
}