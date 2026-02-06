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
  array[n] int temperature;
  int n_temperature;
  vector[n] grazing;
  array[n] int replicate;
  int n_replicate;
}

parameters{
  // Parameters describing mean
  vector[n_species] log_alpha_s; // species intercepts
  vector[n_species] log_mu_s;
  vector[n_species] log_tau_s;
  
  vector[n_temperature] log_mu_t; // temperature deviations
  vector[n_temperature] log_tau_t;
  
  vector[n_temperature] beta_mu; // temperature-specific grazing effect
  vector[n_temperature] beta_tau;
  
  vector[n_replicate] log_alpha_r; // replicate deviation
  vector[n_replicate] log_mu_r;
  vector[n_replicate] log_tau_r;

  // Parameters describing precision
  real<lower=0> epsilon;
  real<lower=0> lambda;
  real<lower=0> theta;
}

model{
  // Priors for parameters describing global mean
  log_alpha_s ~ normal( log(0.004) , 0.2 );
  log_mu_s ~ normal( log(30) , 0.2 );
  log_tau_s ~ normal( log(0.1) , 0.2 );
  
  log_mu_t ~ normal( 0 , 0.2 );
  log_tau_t ~ normal( 0 , 0.2 );
  
  beta_mu ~ normal( 0 , 0.2 );
  beta_tau ~ normal( 0 , 0.2 );
  
  log_alpha_r ~ normal( 0 , 0.2 );
  log_mu_r ~ normal( 0 , 0.2 );
  log_tau_r ~ normal( 0 , 0.2 );

  // Priors for parameters describing global precision
  epsilon ~ gamma( square(4e4) / square(2e4) , 4e4 / square(2e4) );
  lambda ~ exponential( 1 );
  theta ~ gamma( square(500) / square(250) , 500 / square(250) );
  
  // Model
  // Parameters
  vector[n] alpha = exp( 
    log_alpha_s[species] + log_alpha_r[replicate] // alpha cannot vary by treatment
  );
  vector[n] mu = exp(
    log_mu_s[species] + log_mu_t[temperature] + 
    beta_mu[temperature] .* grazing + log_mu_r[replicate]
  );
  vector[n] tau = exp( 
    log_tau_s[species] + log_tau_t[temperature] + 
    beta_tau[temperature] .* grazing + log_tau_r[replicate]
  );
  
  // Function describing mean
  vector[n] m_mu = exp(
      t .* alpha - ( alpha + tau ) .* mu / 5 .* (
        log1p_exp( 5 / mu .* ( t - mu ) ) -
        log1p_exp( -5 )
      )
  );
  
  // Function describing precision (for some reason this form works better here)
  vector[n] nu = ( epsilon - theta ) * exp( -lambda * t ) + theta;
    
  // Beta prime likelihood
  for ( i in 1:n ) m[i] ~ betap( m_mu[i] * ( 1 + nu[i] ) , 2 + nu[i] );
}