data{
  int n;
  vector[n] t;
  vector[n] m;
  array[n] int replicate;
  int n_replicate;
}

parameters{
  // Parameters describing mean
  vector[n_replicate] alpha;
  vector<lower=0>[n_replicate] mu;
  vector<lower=0>[n_replicate] tau;
  
  // Parameters describing rate
  real<lower=0> epsilon;
  real<lower=0> lambda;
  real<lower=0> theta;
}

model{
  // Priors
  alpha ~ normal( 0 , 0.02 );
  mu ~ gamma( square(60) / square(40) , 60 / square(40) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  epsilon ~ gamma( square(2e4) / square(1e4) , 2e4 / square(1e4) );
  lambda ~ exponential( 1 );
  theta ~ gamma( square(250) / square(125) , 250 / square(125) );
  
  // Model
  vector[n] m_mu = exp(
      t .* alpha[replicate] - 
      ( alpha[replicate] + tau[replicate] ) .* mu[replicate] / 5 .* (
        log1p_exp( 5 / mu[replicate] .* ( t - mu[replicate] ) ) - 
        log1p_exp( -5 )
      )
  );
  
  // Here this arrangement works better than theta+exp(log(epsilon-theta)-lambda*t)
  vector[n] beta = ( epsilon - theta ) * exp( -lambda * t ) + theta;

  // Gamma likelihood
  m ~ gamma( m_mu .* beta , beta );
}

generated quantities{
  // Save mean
  vector[n] m_mu = exp(
      t .* alpha[replicate] - 
      ( alpha[replicate] + tau[replicate] ) .* mu[replicate] / 5 .* (
        log1p_exp( 5 / mu[replicate] .* ( t - mu[replicate] ) ) - 
        log1p_exp( -5 )
      )
  );
  
  // Save rate
  vector[n] beta = ( epsilon - theta ) * exp( -lambda * t ) + theta;
  
  // Save pointwise log-likelihood
  vector[n] log_lik;
  for(i in 1:n) log_lik[i] = gamma_lpdf( m[i] | m_mu[i] * beta[i] , beta[i] );
}