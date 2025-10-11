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
  vector<lower=0>[n_treatment] alpha;
  vector<lower=0>[n_treatment] mu;
  vector<lower=0>[n_treatment] tau;
  
  // Parameters describing precision
  real<lower=0> epsilon;
  vector<lower=0>[n_treatment] lambda;
  vector<lower=0>[n_treatment] theta;
}

model{
  // Priors for parameters describing mean
  alpha ~ exponential( 100 );
  mu ~ gamma( square(25) / square(10) , 25 / square(10) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  
  // Priors for parameters describing precision
  epsilon ~ gamma( square(4e4) / square(2e4) , 4e4 / square(2e4) );
  lambda ~ exponential( 1 );
  theta ~ gamma( square(500) / square(250) , 500 / square(250) );
  
  // Model
  // Function describing mean
  vector[n] p_mu = exp(
      t .* alpha[treatment] - 
      ( alpha[treatment] + tau[treatment] ) .* mu[treatment] ./ 5 .* (
        log1p_exp( 5 ./ mu[treatment] .* ( t - mu[treatment] ) ) -
        log1p_exp( -5 )
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