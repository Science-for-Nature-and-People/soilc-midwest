data {
  // Dimensions
  int<lower=0> N;

  // Variables
  vector[N] yield;
}
parameters  {
  real alpha;
  real beta_ar;
  real<lower=0> sigma;
}
model {
  // Uninformative priors
  alpha ~ normal(0,10);
  beta_ar ~ normal(0,10);
  sigma ~ cauchy(0,5);
  
  // Estimate model
  for(n in 2:N)
    yield[n] ~ normal(yield[n-1]*beta_ar + alpha, sigma);
}
/*
generated quantities {
  // Predict new response variable to check against observed data
  vector[N] y_tilde;
  for (n in 1:N)
    y_tilde[n] = normal_rng(counties[n]*beta_counties + ar[n]*beta_ar + alpha, sigma);
}
*/
