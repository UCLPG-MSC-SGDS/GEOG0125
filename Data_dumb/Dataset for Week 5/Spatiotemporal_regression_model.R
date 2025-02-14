rm(list = ls())
gc()

setwd("~/Desktop")

# Load required packages
library(rstan)
library(Matrix)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Simulated data
set.seed(123)
N <- 10            # Number of regions
T <- 5             # Number of time points
K <- 2             # Number of covariates
X <- array(rnorm(N * T * K), dim = c(N, T, K))  # Covariates X1 and X2
E <- runif(N, 100, 200)                         # Population/exposure
beta_true <- c(0.5, -0.3)                       # True beta values
spatial_eta_true <- rnorm(N, 0, 1)              # True spatial effects
temporal_eta_true <- arima.sim(model = list(ar = 0.7), n = T)  # True temporal effects

# Generate response counts using a Poisson model
lambda <- array(NA, dim = c(N, T))
y <- matrix(NA, nrow = N, ncol = T)
for (n in 1:N) {
	for (t in 1:T) {
		log_lambda <- 1 + sum(beta_true * X[n, t, ]) + spatial_eta_true[n] + temporal_eta_true[t] + log(E[n])
		lambda[n, t] <- exp(log_lambda)
		y[n, t] <- rpois(1, lambda[n, t])
	}
}

# Adjacency list for spatial ICAR model
adj <- c(1, 2, 2, 3, 3, 4, 4, 5, 5, 1)  # Example of a ring structure
num_neighbors <- rep(2, N)
num_edges <- length(adj)

# Stan data
stan_data <- list(
	N = N,
	T = T,
	K = K,
	X = X,
	y = y,
	E = E,
	adj = adj,
	num_neighbors = num_neighbors,
	num_edges = num_edges
)

# Fit the model
fit <- stan(file = "Spatiotemporal_regression_model.stan", data = stan_data, iter = 2000, chains = 4)

# Print results
print(fit, pars = c("beta_0", "beta", "spatial_eta", "temporal_eta", "sigma_spatial", "sigma_temporal", "rho"))
