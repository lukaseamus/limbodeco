data{
  int n;
  vector[n] t;
  vector[n] m;
  array[n] int treatment;
  int n_treatment;
}

parameters{
  real log_k_mu;
  real<lower=0> log_k_sigma;
  vector[n_treatment] log_k_z;
  real<lower=0> sigma;
}

transformed parameters{
  // Convert z-scores
  vector[n_treatment] log_k = log_k_z * log_k_sigma + log_k_mu;
}

model{
  // Priors
  log_k_mu ~ normal( log(0.1) , 0.5 );
  log_k_sigma ~ normal( 0 , 0.5 )T[0,];
  log_k_z ~ normal( 0 , 1 );
  sigma ~ exponential( 1 );
  
  // Model
  vector[n] k = exp( log_k[treatment] );
  vector[n] m_mu = exp( -k .* t );

  // Normal likelihood
  m ~ normal( m_mu , sigma );
}