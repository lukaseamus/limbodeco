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
  array[n] int treatment;
  int n_treatment;
}

parameters{
  // Parameters describing mean
  vector[n_treatment] alpha;
  vector<lower=0>[n_treatment] mu;
  real<lower=0> tau;
  
  // Parameters describing precision
  real<lower=0> epsilon;
  vector<lower=0>[n_treatment] lambda;
  vector<lower=0>[n_treatment] theta;
}

model{
  // Priors for parameters describing mean
  alpha ~ normal( 0 , 0.01 );
  mu ~ exponential( 0.01 );
  tau ~ exponential( 10 );
  
  // Priors for parameters describing precision
  epsilon ~ gamma( square(4e4) / square(2e4) , 4e4 / square(2e4) );
  lambda ~ exponential( 10 );
  theta ~ gamma( square(100) / square(50) , 100 / square(50) );
  
  // Model
  // Function describing mean
  vector[n] p_mu = exp(
      t .* alpha[treatment] -
      ( alpha[treatment] + tau ) .* (
        log1p_exp( t - mu[treatment] ) -
        log1p_exp( -mu[treatment] )
      )
    );
  
  // Function describing precision
  vector[n] nu = theta[treatment] + exp(
      log( epsilon - theta[treatment] )
      - lambda[treatment] .* t
    );
  
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
      t[i] * alpha[treatment[i]] -
      ( alpha[treatment[i]] + tau ) * (
        log1p_exp( t[i] - mu[treatment[i]] ) -
        log1p_exp( -mu[treatment[i]] )
      )
    );
  }
  
  // Calculate and save precision
  vector[n] nu;
  for ( i in 1:n ) {
    nu[i] = theta[treatment[i]] + exp(
      log( epsilon - theta[treatment[i]] )
      - lambda[treatment[i]] * t[i]
    );
  }
  
  // Calculate and save log likelihood
  vector[n] log_lik;
  for ( i in 1:n ) {
    log_lik[i] = betap_lpdf( p[i] | p_mu[i] * ( 1 + nu[i] ) , 2 + nu[i] );
  }
}