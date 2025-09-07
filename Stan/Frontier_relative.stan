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
  vector[n] p;
  array[n] int species;
  int n_species;
  array[n] int treatment;
  int n_treatment;
}

parameters{
  // Parameters describing mean
  vector[n_species] alpha;
  matrix<lower=0>[n_species, n_treatment] mu;
  vector<lower=0>[n_species] tau;
  
  // Parameters describing precision
  real<lower=0> nu_max;
  matrix<lower=0>[n_species, n_treatment] nu_beta;
  real<lower=0> nu_min;
}

model{
  // Priors for parameters describing mean
  alpha ~ normal( 0 , 0.01 );
  to_vector(mu) ~ gamma( square(30) / square(15) , 30 / square(15) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );

  // Priors for parameters describing precision
  nu_max ~ gamma( square(1e5) / square(5e4) , 1e5 / square(5e4) );
  to_vector(nu_beta) ~ exponential( 10 );
  nu_min ~ gamma( square(30) / square(30) , 30 / square(30) );
  
  // Model
  // Function describing mean
  vector[n] p_mu;
  for ( i in 1:n ) {
    p_mu[i] = exp(
      t[i] * alpha[species[i]] -
      ( alpha[species[i]] + tau[species[i]] ) * 
      mu[species[i], treatment[i]] / 5 * (
        log1p_exp( 5 / mu[species[i], treatment[i]] * 
                  ( t[i] - mu[species[i], treatment[i]] ) ) -
        log1p_exp( -5 )
      )
    );
  }
  
  // Function describing precision
  vector[n] nu;
  for ( i in 1:n ) {
    nu[i] =  nu_min + exp(
      log( nu_max - nu_min )
      - nu_beta[species[i], treatment[i]] * t[i]
    );
  }
  
  // Beta prime likelihood
  for ( i in 1:n ) {
    p[i] ~ betap( p_mu[i] * ( 1 + nu[i] ) , 2 + nu[i] );
  }
}

generated quantities{
  // Calculate and save mean
  vector[n] p_mu;
  for ( i in 1:n ) {
    p_mu[i] = exp(
      t[i] * alpha[species[i]] -
      ( alpha[species[i]] + tau[species[i]] ) * 
      mu[species[i], treatment[i]] / 5 * (
        log1p_exp( 5 / mu[species[i], treatment[i]] * 
                  ( t[i] - mu[species[i], treatment[i]] ) ) -
        log1p_exp( -5 )
      )
    );
  }
  
  // Calculate and save precision
  vector[n] nu;
  for ( i in 1:n ) {
    nu[i] =  nu_min + exp(
      log( nu_max - nu_min )
      - nu_beta[species[i], treatment[i]] * t[i]
    );
  }
  
  // Calculate and save log likelihood
  vector[n] log_lik;
  for ( i in 1:n ) {
    log_lik[i] = betap_lpdf( p[i] | p_mu[i] * ( 1 + nu[i] ) , 2 + nu[i] );
  }
}