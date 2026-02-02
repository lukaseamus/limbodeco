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
  vector[n] m_mean;
  vector[n] m_sd;
  array[n] int treatment;
  int n_treatment;
}

transformed data{
  // Convert sd to nu because this is easier on the sampler
  vector[n] m_nu = m_mean .* ( 1 + m_mean ) ./ m_sd^2;
}

parameters{
  // Latent variable
  vector<lower=0>[n] m;
  
  // Parameters describing mean
  vector[n_treatment] alpha;
  vector<lower=0>[n_treatment] mu;
  real<lower=0> tau;
  
  // Parameters describing precision
  real<lower=0> epsilon;
  vector<lower=0>[n_treatment] lambda;
  real<lower=0> theta;
}

model{
  // Priors for parameters describing mean
  alpha ~ normal( -0.01 , 0.005 );
  mu ~ gamma( square(150) / square(100) , 150 / square(100) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  
  // Priors for parameters describing precision
  epsilon ~ gamma( square(4e4) / square(2e4) , 4e4 / square(2e4) );
  lambda ~ exponential( 1 );
  theta ~ gamma( square(500) / square(250) , 500 / square(250) );
  
  // Model
  // Function describing mean
  vector[n] m_mu = exp(
      t .* alpha[treatment] - 
      ( alpha[treatment] + tau ) .* mu[treatment] / 5 .* (
        log1p_exp( 5 / mu[treatment] .* ( t - mu[treatment] ) ) -
        log1p_exp( -5 )
      )
  );
  
  // Function describing precision
  vector[n] nu = theta + exp(
      log( epsilon - theta ) - lambda[treatment] .* t
  );
    
  // Beta prime likelihood
  for ( i in 1:n ) {
    m[i] ~ betap( m_mu[i] * ( 1 + nu[i] ) , 2 + nu[i] );
  }
  
  // Beta prime measurement error model
  for ( i in 1:n ) {
    m_mean[i] ~ betap( m[i] * ( 1 + m_nu[i] ), 2 + m_nu[i] );
  }
}