# code for null spatiotemporal model
rm(list = ls())
gc()

setwd("~/Desktop")

# Load the necessary libraries
library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Define the number of spatial locations (N) and time points (T)
N <- 10  # Example: 10 spatial units
T <- 5   # Example: 5 time points

# Example adjacency matrix W for ICAR prior
W <- matrix(0, N, N)
# Define your actual adjacency matrix here

# Fill the adjacency matrix (W)
# Example: Assuming a simple ring structure for demonstration
for (i in 1:N) {
	W[i, (i %% N) + 1] <- 1  # Connect to the next region (cyclic)
	W[i, (i - 2) %% N + 1] <- 1  # Connect to the previous region (cyclic)
}

# Example response counts (y)
y <- matrix(rpois(N * T, lambda = 10), N, T)

# Expected counts or population size (E)
E <- runif(N, min = 50, max = 100)  # Replace with actual data

# Stan data list
stan_data <- list(
	N = N,
	T = T,
	K = N * T,
	W = W,
	y = y,
	E = E,
	tau = 1.0  # Example value for scale parameter
)

# Compile the Stan model
stan_model <- stan_model(file = "spatio_temporal_poisson_icar.stan")

# Fit the model
fit <- sampling(stan_model, data = stan_data, iter = 5000, chains = 6)

# Summarize results
print(fit)

print(fit, pars = c("beta_0", "spatial_eta", "temporal_eta", "log_lambda", "rho", "sigma_eta"), probs = c(0.025, 0.5, 0.975))


prec <- 3
vege <- 5
popn <- 3

suit <- 0.6689*prec + 0.0637*vege + 0.2674*popn
suit