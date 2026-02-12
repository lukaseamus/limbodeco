data{
  int n;
  vector[n] t;
  vector[n] m_mean;
  vector[n] m_sd;
  array[n] int treatment;
  int n_treatment;
}

parameters{
  vector<lower=0>[n] m;
  vector<lower=0>[n_treatment] k;
  real<lower=0> sigma;
}

model{
  // Priors
  k ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  sigma ~ exponential( 1 );
  
  // Model
  vector[n] m_mu = exp( -k[treatment] .* t );

  // Normal likelihood
  m ~ normal( m_mu , sigma );
  
  // Normal measurement error model
  m_mean ~ normal( m , m_sd );
}