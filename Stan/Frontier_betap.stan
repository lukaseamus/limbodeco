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
}

parameters{
  // Parameters describing mean
  real alpha;
  real<lower=0> mu;
  real<lower=0> tau;
  
  // Likelihood precision
  real<lower=0> nu;
}

model{
  // Priors
  alpha ~ normal( 0 , 0.02 );
  mu ~ gamma( square(30) / square(20) , 30 / square(20) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  nu ~ gamma( square(100) / square(50) , 100 / square(50) );
  
  // Model
  vector[n] m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
  );

  // Beta prime likelihood
  for ( i in 1:n ) m[i] ~ betap( m_mu[i] * ( 1 + nu ) , 2 + nu );
}

generated quantities{
  // Save mean
  vector[n] m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
  );
  
  // Save pointwise log-likelihood
  vector[n] log_lik;
  for(i in 1:n) log_lik[i] = betap_lpdf( m[i] | m_mu[i] * ( 1 + nu ) , 2 + nu );
}