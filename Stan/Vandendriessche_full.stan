data{
  int n;
  vector[n] t;
  vector[n] m;
  array[n] int replicate;
  int n_replicate;
}

parameters{
  // Parameters describing mean
  vector<lower=0>[n_replicate] r;
  vector[n_replicate] alpha;
  vector<lower=0>[n_replicate] mu;
  vector<lower=0>[n_replicate] tau;
  
  // Likelihood standard deviation
  real<lower=0> sigma;
}

model{
  // Priors
  r ~ gamma( square(1) / square(0.5) , 1 / square(0.5) );
  alpha ~ normal( 0 , 0.02 );
  mu ~ gamma( square(60) / square(40) , 60 / square(40) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  sigma ~ exponential( 1 );
  
  // Model (full parameterisation)
  vector[n] m_mu = exp(
      t .* alpha[replicate] - 
      ( alpha[replicate] + tau[replicate] ) ./ r[replicate] .* (
        log1p_exp( r[replicate] .* ( t - mu[replicate] ) ) - 
        log1p_exp( -r[replicate] .* mu[replicate] )
      )
  );

  // Normal likelihood
  m ~ normal( m_mu , sigma );
}

generated quantities{
  // Save mean
  vector[n] m_mu = exp(
      t .* alpha[replicate] - 
      ( alpha[replicate] + tau[replicate] ) ./ r[replicate] .* (
        log1p_exp( r[replicate] .* ( t - mu[replicate] ) ) - 
        log1p_exp( -r[replicate] .* mu[replicate] )
      )
  );
  
  // Save pointwise log-likelihood
  vector[n] log_lik;
  for(i in 1:n) log_lik[i] = normal_lpdf( m[i] | m_mu[i] , sigma );
}