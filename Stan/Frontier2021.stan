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
  vector<lower=0>[n_species] alpha;
  matrix<lower=0>[n_species, n_treatment] mu;
  vector<lower=0>[n_species] tau;
  
  // Parameters describing precision
  real<lower=0> epsilon;
  matrix<lower=0>[n_species, n_treatment] lambda;
  vector<lower=0>[n_species] theta;
}

model{
  // Priors for parameters describing mean
  alpha ~ exponential( 100 );
  to_vector(mu) ~ gamma( square(50) / square(30) , 50 / square(30) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );

  // Priors for parameters describing precision
  epsilon ~ gamma( square(4e4) / square(2e4) , 4e4 / square(2e4) );
  to_vector(lambda) ~ exponential( 1 );
  theta ~ gamma( square(500) / square(250) , 500 / square(250) );
  
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
    nu[i] =  theta[species[i]] + exp(
      log( epsilon - theta[species[i]] )
      - lambda[species[i], treatment[i]] * t[i]
    );
  }
  
  // Beta prime likelihood
  for ( i in 1:n ) {
    p[i] ~ betap( p_mu[i] * ( 1 + nu[i] ) , 2 + nu[i] );
  }
}