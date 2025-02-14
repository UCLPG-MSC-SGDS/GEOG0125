
data {
  int<lower=0> N;                          // Number of spatial units
  int<lower=0> Tn;                          // Number of time periods
  int<lower=0> N_edges;                    // Number of spatial edges (Queen contiguity)
  array[N_edges] int<lower=1, upper=N> node1;  // First node in edge
  array[N_edges] int<lower=1, upper=N> node2;  // Second node in edge
  array[N, Tn] int<lower=0> Y;             // Dependent variable: cases per spatial unit & time
  matrix<lower=0>[N, Tn] Off_set;          // Offset variable
}

transformed data {
  matrix[N, Tn] log_Offset = log(Off_set); // Log offset for Poisson regression
}

parameters {
  real alpha;                             // Intercept
  real<lower=0> sigma;                    // Overall standard deviation
  real<lower=0, upper=1> rho_s;           // Spatial variance proportion
  real<lower=0, upper=1> rho_t;           // Temporal variance proportion

  // AR(1) variance parameter
  real<lower=0> tau_t;                    // Standard deviation of the AR(1) process

  // Hierarchical or penalized approach for unstructured effects:
  real<lower=0> sigma_theta;              // Scale for unstructured random effects

  matrix[N, Tn] theta;                     // Unstructured random effects (spatiotemporal noise)
  matrix[N, Tn] phi_s;                     // Spatial structured random effects (ICAR)
  vector[Tn] phi_t;                        // Temporal random effects (AR(1))
}

transformed parameters {
  matrix[N, Tn] combined;

  // Combine spatial and temporal effects
  combined = sqrt(1 - rho_s) * theta + sqrt(rho_s) * phi_s;
  combined += sqrt(rho_t) * rep_matrix(phi_t, N);
}

model {
  // Likelihood (vectorized)
  Y ~ poisson_log(
    to_vector(log_Offset)
    + alpha
    + to_vector(combined) * sigma
  );

  // Priors
  alpha ~ normal(0, 1);
  sigma ~ normal(0, 1);
  rho_s ~ beta(0.5, 0.5);
  rho_t ~ beta(0.5, 0.5);
  tau_t ~ normal(0, 1);        // Prior for AR(1) std

  // Now we use sigma_theta to control unstructured effect size:
  sigma_theta ~ normal(0, 1);  // prior for scale of unstructured effects
  to_vector(theta) ~ normal(0, sigma_theta);

  // Spatial ICAR prior
  for (t in 1:Tn) {
    target += -0.5 * dot_self(phi_s[node1, t] - phi_s[node2, t]);
    sum(phi_s[, t]) ~ normal(0, 0.1 * N); // Weaker sum-to-zero constraint
  }

  // Temporal AR(1) structure
  for (t in 2:Tn) {
    phi_t[t] ~ normal(phi_t[t - 1], tau_t);
  }
  phi_t[1] ~ normal(0, 1);
}

generated quantities {
  matrix[N, Tn] eta;
  matrix[N, Tn] rr_mu;    // Relative risks

  for (t in 1:Tn) {
    eta[, t] = alpha + combined[, t] * sigma;
    rr_mu[, t] = exp(eta[, t]);
  }

  real rr_beta = exp(beta);    // Risk ratio for covariate
  real rr_alpha = exp(alpha);  // Risk ratio for intercept
}

