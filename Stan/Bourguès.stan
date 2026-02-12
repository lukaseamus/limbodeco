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
  real log_alpha_mu;
  real log_mu_mu;
  real log_tau_mu;
  real<lower=0> log_alpha_sigma;
  real<lower=0> log_mu_sigma;
  real<lower=0> log_tau_sigma;
  
  // Parameters describing treatment mean
  vector[n_treatment] log_alpha_z;
  vector[n_treatment] log_mu_z;
  vector[n_treatment] log_tau_z;
  
  // Parameters describing global precision
  real<lower=0> epsilon;
  real log_lambda_mu;
  real log_theta_mu;
  real<lower=0> log_lambda_sigma;
  real<lower=0> log_theta_sigma;
  
  // Parameters describing treatment precision
  vector[n_treatment] log_lambda_z;
  vector[n_treatment] log_theta_z;
}

transformed parameters{
  // Convert z-scores
  vector[n_treatment] log_alpha = log_alpha_z * log_alpha_sigma + log_alpha_mu;
  vector[n_treatment] log_mu = log_mu_z * log_mu_sigma + log_mu_mu;
  vector[n_treatment] log_tau = log_tau_z * log_tau_sigma + log_tau_mu;
  vector[n_treatment] log_lambda = log_lambda_z * log_lambda_sigma + log_lambda_mu;
  vector[n_treatment] log_theta = log_theta_z * log_theta_sigma + log_theta_mu;
}

model{
  // Priors for parameters describing global mean
  log_alpha_mu ~ normal( log(0.005) , 0.2 );
  log_mu_mu ~ normal( log(15) , 0.5 );
  log_tau_mu ~ normal( log(0.1) , 0.5 );
  
  log_alpha_sigma ~ normal( 0 , 0.2 )T[0,];
  log_mu_sigma ~ normal( 0 , 0.5 )T[0,];
  log_tau_sigma ~ normal( 0 , 0.5 )T[0,];
  
  // Priors for parameters describing treatment mean
  log_alpha_z ~ normal( 0 , 1 );
  log_mu_z ~ normal( 0 , 1 );
  log_tau_z ~ normal( 0 , 1 );
  
  // Priors for parameters describing global precision
  epsilon ~ gamma( square(4e4) / square(2e4) , 4e4 / square(2e4) );
  log_lambda_mu ~ normal( log(1) , 0.4 );
  log_theta_mu ~ normal( log(500) , 0.4 );
  
  log_lambda_sigma ~ normal( 0 , 0.4 )T[0,];
  log_theta_sigma ~ normal( 0 , 0.4 )T[0,];
  
  // Priors for parameters describing treatment precision
  log_lambda_z ~ normal( 0 , 1 );
  log_theta_z ~ normal( 0 , 1 );
  
  // Model
  // Parameters
  vector[n] alpha = exp( log_alpha[treatment] );
  vector[n] mu = exp( log_mu[treatment] );
  vector[n] tau = exp( log_tau[treatment] );
  vector[n] lambda = exp( log_lambda[treatment] );
  vector[n] theta = exp( log_theta[treatment] );
  
  // Function describing mean
  vector[n] m_mu = exp(
      t .* alpha - ( alpha + tau ) .* mu / 5 .* (
        log1p_exp( 5 / mu .* ( t - mu ) ) -
        log1p_exp( -5 )
      )
    );
  
  // Function describing precision
  vector[n] nu = theta + exp(
      log( epsilon - theta ) - lambda .* t
  );
    
  // Beta prime likelihood
  for ( i in 1:n ) m[i] ~ betap( m_mu[i] * ( 1 + nu[i] ) , 2 + nu[i] );
}