
data {
  int<lower=1> N;             // Number of spatial units
  int<lower=1> T;             // Number of time points
  int<lower=1> K;             // Total number of observations (N * T)
  matrix[N, N] W;             // Adjacency matrix for spatial units
  
  array[N, T] int<lower=0>y;
  // int<lower=0> y[N, T];       // Count data (response variable)
  vector<lower=0>[N] E;       // Expected cases (offset)
  real<lower=0> tau;          // Scale parameter for ICAR prior
}

parameters {
  vector[N] spatial_eta;      // Latent spatial effects
  real<lower=0> sigma_eta;    // Standard deviation of spatial random effects
  vector[T] temporal_eta;     // Latent temporal effects (time component)
  real<lower=-1, upper=1> rho; // AR(1) coefficient for temporal autocorrelation
  real beta_0;                // Intercept (log rate)
}

model {
  vector[N] spatial_mu;       // Spatial means
  matrix[N, T] mu;            // Mean response (log lambda)

  // ICAR Prior for spatial effects
  for (n in 1:N)
    spatial_eta[n] ~ normal(0, sigma_eta);

  // AR(1) Process for temporal effects
  temporal_eta[1] ~ normal(0, sigma_eta);
  for (t in 2:T)
    temporal_eta[t] ~ normal(rho * temporal_eta[t-1], sqrt(1 - square(rho)));

  // Poisson Likelihood with spatial and temporal random effects
  for (t in 1:T) {
    for (n in 1:N) {
      mu[n, t] = beta_0 + spatial_eta[n] + temporal_eta[t];
      y[n, t] ~ poisson_log(mu[n, t] + log(E[n]));  // Log-linear Poisson model
    }
  }

  // Priors
  beta_0 ~ normal(0, 5);       // Prior for the intercept
  sigma_eta ~ normal(0, 2);    // Prior for the spatial SD
}

generated quantities {
  matrix[N, T] log_lambda;
  for (t in 1:T)
    for (n in 1:N)
      log_lambda[n, t] = beta_0 + spatial_eta[n] + temporal_eta[t];
}
