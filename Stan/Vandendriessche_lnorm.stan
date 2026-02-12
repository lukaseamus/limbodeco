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
  
  // Model
  // I am not exponentiating because the likelihood takes the logarithm of mu
  vector[n] log_m_mu = t .* alpha[replicate] - 
  ( alpha[replicate] + tau[replicate] ) .* mu[replicate] / 5 .* (
    log1p_exp( 5 / mu[replicate] .* ( t - mu[replicate] ) ) - 
    log1p_exp( -5 )
  );

  // Lognormal likelihood
  m ~ lognormal( log_m_mu , sigma );
}

generated quantities{
  // Save mean
  vector[n] log_m_mu = t .* alpha[replicate] - 
  ( alpha[replicate] + tau[replicate] ) .* mu[replicate] / 5 .* (
    log1p_exp( 5 / mu[replicate] .* ( t - mu[replicate] ) ) - 
    log1p_exp( -5 )
  );
  
  // Save pointwise log-likelihood
  vector[n] log_lik;
  for(i in 1:n) log_lik[i] = lognormal_lpdf( m[i] | log_m_mu[i] , sigma );
}