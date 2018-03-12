data {
  // Dimensions
  int<lower=0> N;

  // Variables
  vector[N] covar;
  vector[N] yield_mean;
  vector[N] yield_sd;
}
transformed data {
  // Calculate coefficient of variation
  vector[N] yield_cov;
  yield.cov = yield_sd / yield_mean;
}
parameters  {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  // Uninformative priors
  alpha ~ normal(0,10);
  beta ~ normal(0,10);
  sigma ~ cauchy(0,5);
  
  // Estimate model
  yield.cov ~ normal(covar*beta + alpha, sigma);
}
generated quantities {
  // Predict new response variable to check against observed data
  vector[N] y_tilde;
  for (n in 1:N)
    y_tilde[n] = normal_rng(covar[n]*beta + alpha, sigma);
}
