
data {
  int<lower=0> N;                                          // Number of spatial units
  int<lower=0> T;                                          // Number of time periods
  int<lower=0> N_edges;
  array[N_edges] int<lower=1, upper=N> node1;
  array[N_edges] int<lower=1, upper=N> node2;
  array[N, T] int<lower=0> Y;                               // Dependent variable
  //matrix<lower=0>[N, T] Off_set;                          // Offset variable
  matrix[N, T] Off_set;                           
}

transformed data {
  matrix[N, T] log_Offset = log(Off_set);                   // Log offset for Poisson regression
}

parameters {
  real alpha;                                               // Intercept
  real<lower=0> sigma;                                      // Overall standard deviation
  real<lower=0, upper=1> rho_s;                             // Spatial variance proportion
  real<lower=0, upper=1> rho_t;                             // Temporal variance proportion
  matrix[N, T] theta;                                       // Unstructured random effects (spatiotemporal noise)
  matrix[N, T] phi_s;                                       // Spatial structured random effects (ICAR)
  vector[T] phi_t;                                          // Temporal random effects (AR(1) or ICAR)
  
  // AR(1) variance parameter
  real<lower=0> tau_t;                                      // Standard deviation of the AR(1) process

  // Hierarchical prior for unstructured effects
  real<lower=0> sigma_theta;                                // Scale for unstructured random effects

}

transformed parameters {
  matrix[N, T] combined;

  // Combine spatial and temporal effects
  combined = sqrt(1 - rho_s) * theta + sqrt(rho_s) * phi_s;
  combined += sqrt(rho_t) * rep_matrix(phi_t, N);
}

model {
  // Likelihood
  for (t in 1:T) {
  	Y[, t] ~ poisson_log(alpha + log_Offset[, t] + combined[, t] * sigma);
  }
  
  // Vectorized Poisson likelihood
  //to_vector(Y) ~ poisson_log(to_vector(alpha + log_Offset + combined * sigma));

  // Priors
  alpha ~ normal(0, 1);
  sigma ~ normal(0, 1);
  rho_s ~ beta(0.5, 0.5);
  rho_t ~ beta(0.5, 0.5);
  tau_t ~ normal(0, 1);
  sigma_theta ~ normal(0, 1);
  
  // Unstructured effects regularization
  to_vector(theta) ~ normal(0, sigma_theta);

  // Spatial ICAR prior
  for (t in 1:T) {
    target += -0.5 * dot_self(phi_s[node1, t] - phi_s[node2, t]);
    sum(phi_s[, t]) ~ normal(0, 0.001 * N); // Soft sum-to-zero constraint
  }
  // Temporal ICAR prior (or AR(1) structure)
  phi_t[1] ~ normal(0, 1);                                       // Prior for first temporal effect
  for (t in 2:T) {
    phi_t[t] ~ normal(phi_t[t - 1], tau_t);                          // Simple AR(1)-like structure
  	}
  // Unstructured random effects
  //to_vector(theta) ~ normal(0, 1);
}

generated quantities {
  matrix[N, T] eta;
  matrix[N, T] rr_mu;    // Relative risks

  for (t in 1:T) {
    eta[, t] = alpha + combined[, t] * sigma;
    rr_mu[, t] = exp(eta[, t]);
  }
  real rr_alpha = exp(alpha); // Risk ratio for intercept
}

