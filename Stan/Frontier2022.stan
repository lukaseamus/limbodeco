functions{
  // Beta prime log probability density function
  real betap_lpdf( real y , real alpha , real beta ) {
    return ( alpha - 1 ) * log( y )
    - ( alpha + beta ) * log1p( y ) -
    lbeta( alpha , beta );
  }
}

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
  // Parameters describing global mean
  real log_alpha_mu;
  real log_mu_mu;
  real log_tau_mu;
  real log_beta_mu; // beta is the depth effect on log mu
  
  real<lower=0> log_alpha_sigma_s; // species standard deviations
  real<lower=0> log_mu_sigma_s;
  real<lower=0> log_tau_sigma_s;
  real<lower=0> log_alpha_sigma_r; // replicate standard deviations
  real<lower=0> log_mu_sigma_r;
  real<lower=0> log_tau_sigma_r;
  real<lower=0> log_beta_sigma; // species standard deviation
  
  // Parameters describing species mean
  vector[n_species] log_alpha_z_s; // z-scores
  vector[n_species] log_mu_z_s;
  vector[n_species] log_tau_z_s;

  vector[n_replicate] log_alpha_z_r;
  vector[n_replicate] log_mu_z_r;
  vector[n_replicate] log_tau_z_r;
  
  vector[n_species] log_beta_z;
  
  /// Parameters describing global precision
  real<lower=0> epsilon;
  real<lower=0> lambda;
  real<lower=0> theta;
}

transformed parameters{
  // Convert z-scores
  vector[n_species] log_alpha_s = log_alpha_z_s * log_alpha_sigma_s + log_alpha_mu;
  vector[n_species] log_mu_s = log_mu_z_s * log_mu_sigma_s + log_mu_mu;
  vector[n_species] log_tau_s = log_tau_z_s * log_tau_sigma_s + log_tau_mu;
  
  vector[n_replicate] log_alpha_r = log_alpha_z_r * log_alpha_sigma_r + 0;
  vector[n_replicate] log_mu_r = log_mu_z_r * log_mu_sigma_r + 0;
  vector[n_replicate] log_tau_r = log_tau_z_r * log_tau_sigma_r + 0;
  
  vector[n_species] log_beta = log_beta_z * log_beta_sigma + log_beta_mu;
}

model{
  // Priors for parameters describing global mean
  log_alpha_mu ~ normal( log(0.004) , 0.2 );
  log_mu_mu ~ normal( log(100) , 0.05 );
  log_tau_mu ~ normal( log(0.12) , 0.2 );
  log_beta_mu ~ normal( log(0.25) , 0.2 );
  
  log_alpha_sigma_s ~ normal( 0 , 0.2 )T[0,];
  log_mu_sigma_s ~ normal( 0 , 0.05 )T[0,];
  log_tau_sigma_s ~ normal( 0 , 0.2 )T[0,];
  log_alpha_sigma_r ~ normal( 0 , 0.2 )T[0,];
  log_mu_sigma_r ~ normal( 0 , 0.05 )T[0,];
  log_tau_sigma_r ~ normal( 0 , 0.2 )T[0,];
  log_beta_sigma ~ normal( 0 , 0.2 )T[0,];
  
  // Priors for parameters describing species mean
  log_alpha_z_s ~ normal( 0 , 1 );
  log_mu_z_s ~ normal( 0 , 1 );
  log_tau_z_s ~ normal( 0 , 1 );
  log_alpha_z_r ~ normal( 0 , 1 );
  log_mu_z_r ~ normal( 0 , 1 );
  log_tau_z_r ~ normal( 0 , 1 );
  log_beta_z ~ normal( 0 , 1 );

  /// Priors for parameters describing global precision
  epsilon ~ gamma( square(4e4) / square(2e4) , 4e4 / square(2e4) );
  lambda ~ exponential( 1 );
  theta ~ gamma( square(500) / square(250) , 500 / square(250) );
  
  // Model
  // Parameters
  vector[n] beta = exp( log_beta[species] );
  vector[n] alpha = exp( log_alpha_s[species] + log_alpha_r[replicate] );
  vector[n] mu = exp( log_mu_s[species] + log_mu_r[replicate] - beta .* depth );
  vector[n] tau = exp( log_tau_s[species] + log_tau_r[replicate] );
  
  // Function describing mean
  vector[n] m_mu = exp(
      t .* alpha - ( alpha + tau ) .* mu / 5 .* (
        log1p_exp( 5 / mu .* ( t - mu ) ) -
        log1p_exp( -5 )
      )
  );
  
  // Function describing precision
  vector[n] nu = theta + exp( log( epsilon - theta ) - lambda * t );
    
  // Beta prime likelihood
  for ( i in 1:n ) m[i] ~ betap( m_mu[i] * ( 1 + nu[i] ) , 2 + nu[i] );
}