
functions {
  // Function to create the precision matrix for ICAR model
  matrix precision_matrix(int N, int num_edges, int[] adj, int[] num_neighbors) {
    matrix[N, N] Q = rep_matrix(0, N, N);  // Initialize precision matrix
    for (i in 1:num_edges) {
      int n1 = adj[2*i-1];
      int n2 = adj[2*i];
      Q[n1, n1] += 1;
      Q[n2, n2] += 1;
      Q[n1, n2] -= 1;
      Q[n2, n1] -= 1;
    }
    return Q;
  }
}

data {
  int<lower=1> N;               // Number of regions
  int<lower=1> T;               // Number of time points
  int<lower=1> K;               // Number of covariates
  matrix[N, K] X[N, T];          // Covariate matrix (N regions by T time points)
  int<lower=0> y[N, T];          // Response counts for each region and time
  vector<lower=0>[N] E;          // Exposure or population for each region
  int<lower=0> adj[N*2];            // Adjacency list for ICAR model
  int<lower=0> num_neighbors[N];  // Number of neighbors for each region
  int<lower=0> num_edges;        // Total number of edges in adjacency list
}

parameters {
  real beta_0;                   // Intercept
  vector[K] beta;                // Coefficients for covariates
  vector[N] spatial_eta;          // Spatial random effects
  vector[T] temporal_eta;         // Temporal random effects
  real<lower=0> sigma_spatial;    // Spatial standard deviation
  real<lower=0> sigma_temporal;   // Temporal standard deviation
  real<lower=-1, upper=1> rho;    // AR(1) correlation for temporal effects
}

model {
  // Priors
  beta_0 ~ normal(0, 5);          // Prior for intercept
  beta ~ normal(0, 2);            // Prior for covariates
  sigma_spatial ~ normal(0, 2);   // Prior for spatial std deviation
  sigma_temporal ~ normal(0, 2);  // Prior for temporal std deviation
  rho ~ uniform(-1, 1);           // Prior for AR(1) correlation

  // ICAR for spatial random effects
  spatial_eta ~ multi_normal_prec(0, sigma_spatial * precision_matrix(adj, num_neighbors));

  // AR(1) process for temporal random effects
  temporal_eta[1] ~ normal(0, sigma_temporal);
  for (t in 2:T)
    temporal_eta[t] ~ normal(rho * temporal_eta[t-1], sigma_temporal);

  // Likelihood
  for (n in 1:N)
    for (t in 1:T)
      y[n, t] ~ poisson_log(beta_0 + dot_product(beta, X[n, t]) +
                            spatial_eta[n] + temporal_eta[t] + log(E[n]));
}

generated quantities {
  matrix[N, T] log_lambda;
  for (t in 1:T)
    for (n in 1:N)
      log_lambda[n, t] = beta_0 + dot_product(beta, X[n, t]) + spatial_eta[n] + temporal_eta[t];
}
