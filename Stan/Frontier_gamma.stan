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
  
  // Likelihood scale
  real<lower=0> theta;
}

model{
  // Priors
  alpha ~ normal( 0 , 0.02 );
  mu ~ gamma( square(30) / square(20) , 30 / square(20) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  theta ~ exponential( 1 );
  
  // Model
  vector[n] m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
  );

  // Gamma likelihood
  m ~ gamma( m_mu / theta , 1 / theta );
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
  for(i in 1:n) log_lik[i] = gamma_lpdf( m[i] | m_mu[i] / theta , 1 / theta );
}