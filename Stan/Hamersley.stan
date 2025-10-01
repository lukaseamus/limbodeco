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
  vector[n] p_mean;
  vector[n] p_sd;
  array[n] int treatment;
  int n_treatment;
}

parameters{
  // Latent variable describing true, unobserved proportion
  vector<lower=0>[n] p;
  
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
  mu ~ gamma( square(10) / square(6) , 10 / square(6) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  
  // Priors for parameters describing precision
  epsilon ~ gamma( square(4e4) / square(2e4) , 4e4 / square(2e4) );
  lambda ~ exponential( 10 );
  theta ~ gamma( square(100) / square(50) , 100 / square(50) );
  
  // Model
  // Function describing mean
  vector[n] p_mu = exp(
      t .* alpha[treatment] - 
      ( alpha[treatment] + tau ) .* mu[treatment] ./ 5 .* (
        log1p_exp( 5 ./ mu[treatment] .* ( t - mu[treatment] ) ) -
        log1p_exp( -5 )
      )
    );
  
  // Function describing precision
  vector[n] nu = theta[treatment] + exp(
      log( epsilon - theta[treatment] )
      - lambda[treatment] .* t
    );
    
  // Beta prime likelihood (nu parameterisation)
  for ( i in 1:n ) {
    p[i] ~ betap( p_mu[i] * ( 1 + nu[i] ) , 2 + nu[i] );
  }
  
  for ( i in 1:n ) {
    p_mean[i] ~ normal( p[i] , p_sd[i] );
  }
  // Beta prime measurement error model (sigma parameterisation)
  // for ( i in 1:n ) {
  //   p_mean[i] ~ betap( p[i] * ( 1 + p[i] * ( 1 + p[i] ) / square(p_sd[i]) ) , 
  //                      2 + p[i] * ( 1 + p[i] ) / square(p_sd[i]) );
  // }
}