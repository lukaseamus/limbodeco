data{
  int n;
  vector[n] t;
  vector[n] m;
  array[n] int species;
  int n_species;
  array[n] int replicate;
  int n_replicate;
  vector[n] depth;
}

parameters{
  real log_k_mu;
  real<lower=0> log_k_sigma_s;
  real<lower=0> log_k_sigma_r;
  
  real beta_mu; // beta is the depth effect on log k
  real<lower=0> beta_sigma;
  
  vector[n_species] log_k_z_s;
  vector[n_replicate] log_k_z_r;
  vector[n_species] beta_z;
  
  real<lower=0> sigma;
}

transformed parameters{
  // Convert z-scores
  vector[n_species] log_k_s = log_k_z_s * log_k_sigma_s + log_k_mu;
  vector[n_replicate] log_k_r = log_k_z_r * log_k_sigma_r + 0;
  vector[n_species] beta = beta_z * beta_sigma + beta_mu;
}

model{
  // Priors
  log_k_mu ~ normal( log(0.12) , 1 );
  log_k_sigma_s ~ normal( 0 , 1 )T[0,];
  log_k_sigma_r ~ normal( 0 , 1 )T[0,];
  beta_mu ~ normal( 0 , 0.3 );
  beta_sigma ~ normal( 0 , 0.3 )T[0,];
  log_k_z_s ~ normal( 0 , 1 );
  log_k_z_r ~ normal( 0 , 1 );
  beta_z ~ normal( 0 , 1 );
  sigma ~ exponential( 1 );
  
  // Model
  vector[n] k = exp(
    log_k_s[species] + log_k_r[replicate] + beta[species] .* depth
  );
  vector[n] m_mu = exp( -k .* t );

  // Normal likelihood
  m ~ normal( m_mu , sigma );
}