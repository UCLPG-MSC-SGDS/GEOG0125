
data {
	int<lower=0> k;                         // Number of variables
  int<lower=0> N;                         // Number of observations
  int<lower=0> obesity[N];                // Number of those obese in an MSOA
  int<lower=0> total[N];                  // Total number of children in an MSOA
  matrix[N, k] X;                         // IMDMSOA and RESTCAT in matrix
}

parameters {
  vector[k] beta;                         // Regression coefficients for IMDMSOA and RESTCAT
}

model {
  vector[N] theta;                        // Estimated probabilities for obesity
  theta = inv_logit(X * beta);            // Logistic transformation of the linear predictor
  beta ~ normal(0, 1);                    // Prior distribution for regression coefficients

  // Likelihood: binomial distribution
  for (i in 1:N) {
    obesity[i] ~ binomial(total[i], theta[i]);
  }
}

