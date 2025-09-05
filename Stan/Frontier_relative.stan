data{
  int n;
  vector[n] t;
  vector[n] p;
  array[n] int species;
  int n_species;
  array[n] int treatment;
  int n_treatment;
}

parameters{
  vector[n_species] alpha;
  matrix<lower=0>[n_species, n_treatment] mu;
  vector<lower=0>[n_species] tau;
  real<lower=0> theta;
}

model{
  // Priors
  alpha ~ normal( 0 , 0.01 );
  to_vector(mu) ~ gamma( square(30) / square(15) , 30 / square(15) );
  tau ~ gamma( square(0.1) / square(0.05) , 0.1 / square(0.05) );
  theta ~ exponential( 10 );
  
  // Model
  vector[n] p_mu;
  for ( i in 1:n ) {
    p_mu[i] = exp(
      t[i] * alpha[species[i]] -
      ( alpha[species[i]] + tau[species[i]] ) * 
      mu[species[i], treatment[i]] / 5 * (
        log1p_exp( 5 / mu[species[i], treatment[i]] * 
                  ( t[i] - mu[species[i], treatment[i]] ) ) -
        log1p_exp( -5 )
      )
  );
  }
  
  // Gamma likelihood
  // p ~ gamma( p_mu / theta , 1 / theta );
  p ~ normal( p_mu , theta );
}

// generated quantities{
//   vector[n] p_mu;
//   for ( i in 1:n ) {
//     p_mu[i] = exp(
//       t[i] * alpha[species[i]] -
//       ( alpha[species[i]] + tau[species[i]] ) *
//       mu[species[i], treatment[i]] / 5 * (
//         log1p_exp( 5 / mu[species[i], treatment[i]] *
//                   ( t[i] - mu[species[i], treatment[i]] ) ) -
//         log1p_exp( -5 )
//       )
//   );
//   }
// 
//   vector[n] log_lik;
//   for ( i in 1:n ) {
//     log_lik[i] = gamma_lpdf( p[i] | p_mu[i] / theta , 1 / theta );
//   }
// }