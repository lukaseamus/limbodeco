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
  array[n] int treatment;
  int n_treatment;
}

parameters{
  // Parameters describing global mean
  real alpha_mu;
  real log_mu_mu;
  real log_tau_mu;
  real<lower=0> alpha_sigma;
  real<lower=0> log_mu_sigma;
  real<lower=0> log_tau_sigma;
  
  // Parameters describing treatment mean
  vector[n_treatment] alpha_z; // z-scores
  vector[n_treatment] log_mu_z;
  vector[n_treatment] log_tau_z;
  
  // Parameters describing global precision
  real<lower=0> epsilon;
  real<lower=0> lambda;
  real<lower=0> theta;
}

transformed parameters{
  // Convert z-scores
  vector[n_treatment] alpha = alpha_z * alpha_sigma + alpha_mu;
  vector[n_treatment] log_mu = log_mu_z * log_mu_sigma + log_mu_mu;
  vector[n_treatment] log_tau = log_tau_z * log_tau_sigma + log_tau_mu;
}

model{
  // Priors for parameters describing global mean
  alpha_mu ~ normal( 0 , 0.01 );
  log_mu_mu ~ normal( log(10) , 0.5 );
  log_tau_mu ~ normal( log(0.2) , 0.5 );
  alpha_sigma ~ normal( 0 , 0.01 )T[0,]; // half-normal priors
  log_mu_sigma ~ normal( 0 , 0.5 )T[0,];
  log_tau_sigma ~ normal( 0 , 0.5 )T[0,];
  
  // Priors for parameters describing treatment mean
  alpha_z ~ normal( 0 , 1 ); // standard normal prior for z-scores
  log_mu_z ~ normal( 0 , 1 );
  log_tau_z ~ normal( 0 , 1 );
  
  // Priors for parameters describing global precision
  epsilon ~ gamma( square(4e4) / square(2e4) , 4e4 / square(2e4) );
  lambda ~ exponential( 1 );
  theta ~ gamma( square(500) / square(250) , 500 / square(250) );
  
  // Model
  // Parameters
  vector[n] a = alpha[treatment];
  vector[n] mu = exp( log_mu[treatment] );
  vector[n] tau = exp( log_tau[treatment] );
  
  // Function describing mean
  vector[n] m_mu = exp(
      t .* a - ( a + tau ) .* mu / 5 .* (
        log1p_exp( 5 / mu .* ( t - mu ) ) -
        log1p_exp( -5 )
      )
  );
  
  // Function describing precision
  vector[n] nu = theta + exp(
      log( epsilon - theta ) - lambda * t
  );
    
  // Beta prime likelihood
  for ( i in 1:n ) m[i] ~ betap( m_mu[i] * ( 1 + nu[i] ) , 2 + nu[i] );
}