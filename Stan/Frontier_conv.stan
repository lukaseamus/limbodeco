data{
  int n;
  vector[n] t;
  vector[n] m;
}

parameters{
  real<lower=0> k;
  real<lower=0> sigma;
}

model{
  // Priors
  k ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  sigma ~ exponential( 1 );
  
  // Model
  vector[n] m_mu = exp( -k * t );

  // Normal likelihood
  m ~ normal( m_mu , sigma );
}

generated quantities{
  // Save mean
  vector[n] m_mu = exp( -k * t );
  
  // Save pointwise log-likelihood
  vector[n] log_lik;
  for(i in 1:n) log_lik[i] = normal_lpdf( m[i] | m_mu[i] , sigma );
}