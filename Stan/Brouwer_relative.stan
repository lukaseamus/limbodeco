data{
  int n;
  vector[n] t;
  vector[n] p;
  array[n] int treatment;
  int n_treatment;
}

parameters{
  vector[n_treatment] alpha;
  vector<lower=0>[n_treatment] mu;
  real<lower=0> tau;
  real<lower=0> theta;
}

model{
  // Priors
  alpha ~ normal( 0 , 0.01 );
  mu ~ exponential( 0.01 );
  tau ~ exponential( 10 );
  theta ~ exponential( 10 );
  
  // Model
  vector[n] p_mu = exp(
      t .* alpha[treatment] - 
      ( alpha[treatment] + tau ) .* mu[treatment] ./ 5 .* (
        log1p_exp( 5 ./ mu[treatment] .* ( t - mu[treatment] ) ) -
        log1p_exp( -5 )
      )
  );
  
  // Gamma likelihood
  p ~ gamma( p_mu / theta , 1 / theta );
}

generated quantities{
  vector[n] p_mu;
  for ( i in 1:n ) {
    p_mu[i] = exp(
      t[i] * alpha[treatment[i]] -
      ( alpha[treatment[i]] + tau ) * mu[treatment[i]] / 5 * (
        log1p_exp( 5 / mu[treatment[i]] * ( t[i] - mu[treatment[i]] ) ) -
        log1p_exp( -5 )
      )
  );
  }
  
  vector[n] log_lik;
  for ( i in 1:n ) {
    log_lik[i] = gamma_lpdf( p[i] | p_mu[i] / theta , 1 / theta );
  }
}