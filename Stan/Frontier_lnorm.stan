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
  
  // Likelihood standard deviation
  real<lower=0> sigma;
}

model{
  // Priors
  alpha ~ normal( 0 , 0.02 );
  mu ~ gamma( square(30) / square(20) , 30 / square(20) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  sigma ~ exponential( 1 );
  
  // Model
  // I am not exponentiating because the likelihood takes the logarithm of mu
  vector[n] log_m_mu = t * alpha - ( alpha + tau ) * mu / 5 * (
    log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
  );

  // Lognormal likelihood
  m ~ lognormal( log_m_mu , sigma );
}

generated quantities{
  // Save mean
  vector[n] log_m_mu = t * alpha - ( alpha + tau ) * mu / 5 * (
    log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
  );
  
  // Save pointwise log-likelihood
  vector[n] log_lik;
  for(i in 1:n) log_lik[i] = lognormal_lpdf( m[i] | log_m_mu[i] , sigma );
}