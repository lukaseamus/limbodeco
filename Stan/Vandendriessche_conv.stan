data{
  int n;
  vector[n] t;
  vector[n] m;
  array[n] int replicate;
  int n_replicate;
}

parameters{
  vector<lower=0>[n_replicate] k;
  real<lower=0> sigma;
}

model{
  // Priors
  k ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  sigma ~ exponential( 1 );
  
  // Model
  vector[n] m_mu = exp( -k[replicate] .* t );

  // Normal likelihood
  m ~ normal( m_mu , sigma );
}

generated quantities{
  // Save mean
  vector[n] m_mu = exp( -k[replicate] .* t );
  
  // Save pointwise log-likelihood
  vector[n] log_lik;
  for(i in 1:n) log_lik[i] = normal_lpdf( m[i] | m_mu[i] , sigma );
}