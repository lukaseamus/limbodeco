data{
  int n;
  vector[n] t;
  vector[n] m;
}

parameters{
  // Parameters describing mean
  real<lower=0> r;
  real alpha;
  real<lower=0> mu;
  real<lower=0> tau;
  
  // Likelihood standard deviation
  real<lower=0> sigma;
}

model{
  // Priors
  r ~ gamma( square(1) / square(0.5) , 1 / square(0.5) );
  alpha ~ normal( 0 , 0.02 );
  mu ~ gamma( square(30) / square(20) , 30 / square(20) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  sigma ~ exponential( 1 );
  
  // Model (full parameterisation)
  vector[n] m_mu = exp(
      t * alpha - ( alpha + tau ) / r * (
        log1p_exp( r * ( t - mu ) ) - log1p_exp( -r * mu )
      )
  );

  // Normal likelihood
  m ~ normal( m_mu , sigma );
}

generated quantities{
  // Save mean
  vector[n] m_mu = exp(
      t * alpha - ( alpha + tau ) / r * (
        log1p_exp( r * ( t - mu ) ) - log1p_exp( -r * mu )
      )
  );
  
  // Save pointwise log-likelihood
  vector[n] log_lik;
  for(i in 1:n) log_lik[i] = normal_lpdf( m[i] | m_mu[i] , sigma );
}