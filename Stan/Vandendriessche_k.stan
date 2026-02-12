data{
  int n;
  vector[n] t;
  vector[n] m;
  array[n] int species;
  int n_species;
  array[n] int temperature;
  int n_temperature;
  vector[n] grazing;
  array[n] int replicate;
  int n_replicate;
}

parameters{
  vector[n_species] log_k_s;
  vector[n_temperature] log_k_t;
  vector[n_temperature] beta;
  vector[n_replicate] log_k_r;
  real<lower=0> sigma;
}

model{
  // Priors
  log_k_s ~ normal( log(0.1) , 1 );
  log_k_t ~ normal( 0 , 1 );
  beta ~ normal( 0 , 1 );
  log_k_r ~ normal( 0 , 1 );
  sigma ~ exponential( 1 );
  
  // Model
  vector[n] k = exp( 
    log_k_s[species] + log_k_t[temperature] + 
    beta[temperature] .* grazing + log_k_r[replicate]
  );
  vector[n] m_mu = exp( -k .* t );

  // Normal likelihood
  m ~ normal( m_mu , sigma );
}