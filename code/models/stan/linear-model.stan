data {
  // Dimensions
  int<lower=0> N;
  int<lower=0> K;
  
  // Variables
  matrix[N,K] x;
  vector[N] y;
}
transformed data{
  // Define transformed matrix
  matrix[N,K] x_std;
  
  // Transform data matrix to standardized
  /*for (k in 1:2) // Centering binary variables
    x_std[,k] = (x[,k] - mean(x[,k]));
  */
  for (k in 1:K) // Standardizing quantitative variables
    x_std[,k] = (x[,k] - mean(x[,k])) / (2*sd(x[,k]));
}
parameters  {
  // Define standardized parameters
  real alpha_std;             // Intercept
  vector[K] beta_std;         // Fixed effects
  real<lower=0> sigma_y;      // FE variance
}
model {
  // Uninformed priors
  alpha_std ~ normal(0,10);
  beta_std ~ normal(0,10);
  sigma_y ~ cauchy(0,5);

  // Model fit with lognormal response
  y ~ lognormal(x_std*beta_std + alpha_std, sigma_y);
}
generated quantities {
  vector[N] y_tilde; // Vector for predicted data
  vector[K] beta;    // Vector for natural coefficients
  
  // Predict data from model parameters
  for(n in 1:N)
    y_tilde[n] = lognormal_rng(x_std[n]*beta_std + alpha_std, sigma_y);
  
  // Generate natural coefficients    
  for(k in 1:K)
    beta[k] = beta_std[k] * sd(y) / sd(x[,k]);
}
