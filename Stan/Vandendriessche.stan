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
  // array[n] int species;
  // int n_species;
  array[n] int replicate;
  int n_replicate;
  // vector[n] temperature;
  // vector[n] grazing;
}

parameters{
  // Parameters describing global mean
  vector[n_replicate] log_alpha;
  vector[n_replicate] log_mu;
  vector[n_replicate] log_tau;
  
  // real log_alpha_mu;
  // real log_mu_mu;
  // real log_tau_mu;
  // real log_beta_mu; // beta is the temperature effect on log mu
  // real log_gamma_mu; // gamma is the grazing effect on log mu
  // 
  // real<lower=0> log_alpha_sigma_s; // species standard deviations
  // real<lower=0> log_mu_sigma_s;
  // real<lower=0> log_tau_sigma_s;
  // 
  // real<lower=0> log_alpha_sigma_r; // replicate standard deviations
  // real<lower=0> log_mu_sigma_r;
  // real<lower=0> log_tau_sigma_r;
  // 
  // real<lower=0> log_beta_sigma; // species standard deviations
  // real<lower=0> log_gamma_sigma;
  // 
  // // Parameters describing species mean
  // vector[n_species] log_alpha_z_s; // z-scores
  // vector[n_species] log_mu_z_s;
  // vector[n_species] log_tau_z_s;
  // 
  // vector[n_replicate] log_alpha_z_r;
  // vector[n_replicate] log_mu_z_r;
  // vector[n_replicate] log_tau_z_r;
  // 
  // vector[n_species] log_beta_z;
  // vector[n_species] log_gamma_z;
  
  // real log_beta;
  // real log_gamma;
  
  /// Parameters describing global precision
  // real<lower=0> epsilon;
  // real<lower=0> lambda;
  // real<lower=0> theta;
  real<lower=0> nu;
}

// transformed parameters{
//   // Convert z-scores
//   vector[n_species] log_alpha_s = log_alpha_z_s * log_alpha_sigma_s + log_alpha_mu;
//   vector[n_species] log_mu_s = log_mu_z_s * log_mu_sigma_s + log_mu_mu;
//   vector[n_species] log_tau_s = log_tau_z_s * log_tau_sigma_s + log_tau_mu;
//   
  // vector[n_replicate] log_alpha_r = log_alpha_z_r * log_alpha_sigma_r + log_alpha_mu;
  // vector[n_replicate] log_mu_r = log_mu_z_r * log_mu_sigma_r + log_mu_mu;
  // vector[n_replicate] log_tau_r = log_tau_z_r * log_tau_sigma_r + log_tau_mu;
//   
//   vector[n_species] log_beta = log_beta_z * log_beta_sigma + log_beta_mu;
//   vector[n_species] log_gamma = log_gamma_z * log_gamma_sigma + log_gamma_mu;
// }

model{
  // Priors for parameters describing global mean
  // log_beta ~ normal( log(0.4) , 0.1 );
  // log_gamma ~ normal( log(1) , 0.1 );
  
  log_alpha ~ normal( log(0.005) , 0.1 );
  log_mu ~ normal( log(20) , 0.1 );
  log_tau ~ normal( log(0.1) , 0.1 );
  // 
  // log_beta_mu ~ normal( log(0.2) , 0.05 );
  // log_gamma_mu ~ normal( log(1) , 0.05 );
  // 
  // log_alpha_sigma_s ~ normal( 0 , 0.05 )T[0,];
  // log_mu_sigma_s ~ normal( 0 , 0.05 )T[0,];
  // log_tau_sigma_s ~ normal( 0 , 0.05 )T[0,];
  // 
  // log_alpha_sigma_r ~ normal( 0 , 0.1 )T[0,];
  // log_mu_sigma_r ~ normal( 0 , 0.1 )T[0,];
  // log_tau_sigma_r ~ normal( 0 , 0.1 )T[0,];
  // 
  // log_beta_sigma ~ normal( 0 , 0.05 )T[0,];
  // log_gamma_sigma ~ normal( 0 , 0.05 )T[0,];
  
  // Priors for parameters describing species mean
  // log_alpha_z_s ~ normal( 0 , 1 );
  // log_mu_z_s ~ normal( 0 , 1 );
  // log_tau_z_s ~ normal( 0 , 1 );
  // 
  // log_alpha_z_r ~ normal( 0 , 1 );
  // log_mu_z_r ~ normal( 0 , 1 );
  // log_tau_z_r ~ normal( 0 , 1 );
  // 
  // log_beta_z ~ normal( 0 , 1 );
  // log_gamma_z ~ normal( 0 , 1 );

  /// Priors for parameters describing global precision
  // epsilon ~ gamma( square(4e4) / square(2e4) , 4e4 / square(2e4) );
  // lambda ~ exponential( 1 );
  // theta ~ gamma( square(500) / square(250) , 500 / square(250) );
  nu ~ gamma( square(100) / square(50) , 100 / square(50) );
  
  // Model
  // Parameters
  vector[n] alpha = exp( log_alpha[replicate] );
  vector[n] mu = exp(
    log_mu[replicate] //- exp( log_beta ) * temperature - exp( log_gamma ) * grazing
  );
  vector[n] tau = exp( log_tau[replicate] );
  
  // vector[n] beta = exp( log_beta[species] );
  // vector[n] gamma = exp( log_gamma[species] );
  // vector[n] alpha = exp( log_alpha_s[species] + log_alpha_r[replicate] );
  // vector[n] mu = exp(
  //   log_mu_s[species] + log_mu_r[replicate] - 
  //   beta .* temperature - gamma .* grazing
  // );
  // vector[n] tau = exp( log_tau_s[species] + log_tau_r[replicate] );
  
  // Function describing mean
  vector[n] m_mu = exp(
      t .* alpha - ( alpha + tau ) .* mu / 5 .* (
        log1p_exp( 5 / mu .* ( t - mu ) ) -
        log1p_exp( -5 )
      )
  );
  
  // Function describing precision
  // Here this arrangement works better than theta+exp(log(epsilon-theta)-lambda*t)
  // vector[n] nu = ( epsilon - theta ) * exp( -lambda * t ) + theta;
    
  // Beta prime likelihood
  for ( i in 1:n ) m[i] ~ betap( m_mu[i] * ( 1 + nu ) , 2 + nu );
}