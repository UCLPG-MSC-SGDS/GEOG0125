
data {
  int<lower=0> N;                                                        // number of spatial units or neighbourhoods
  int<lower=0> N_edges;                                                  // number of edges connecting adjacent areas using Queens contiguity
  array[N_edges] int<lower=1, upper=N> node1;                            // list of index areas showing which spatial units are neighbours
  array[N_edges] int<lower=1, upper=N> node2;                            // list of neighbouring areas showing the connection to index spatial unit
  array[N] int<lower=0> Y;                                               // dependent variable
  vector<lower=0>[N] X;                                                  // Single independent variable
  vector<lower=0>[N] Off_set;                                            // offset variable
}

transformed data {
    vector[N] log_Offset = log(Off_set);                                 // use the expected cases as an offset and add to the regression model
}

parameters {
  real alpha;                                                            // intercept
  real beta;                                                             // covariates
  real<lower=0> sigma;                                                   // overall standard deviation
  real<lower=0, upper=1> rho;                                            // proportion unstructured vs. spatially structured variance
  vector[N] theta;                                                       // unstructured random effects
  vector[N] phi;                                                         // structured spatial random effects
}

transformed parameters {
  vector[N] combined;                                                    // values derived from adding the unstructure and structured effect of each area
  combined = sqrt(1 - rho) * theta + sqrt(rho) * phi;                    // formulation for the combined random effect
}

model {
  Y ~ poisson_log(log_Offset + alpha + X * beta + combined * sigma);    // likelihood function: multivariable Poisson ICAR regression model
                                                                        // setting priors
  alpha ~ normal(0.0, 1.0);                                             // prior for alpha: weakly informative
  beta ~ normal(0.0, 1.0);                                              // prior for betas: weakly informative
  theta ~ normal(0.0, 1.0);                                             // prior for theta: weakly informative
  sigma ~ normal(0.0, 1.0);                                             // prior for sigma: weakly informative
  rho ~ beta(0.5, 0.5);                                                 // prior for rho
  target += -0.5 * dot_self(phi[node1] - phi[node2]);                   // calculates the spatial weights
  sum(phi) ~ normal(0, 0.001 * N);                                      // priors for phi
}

generated quantities {
  vector[N] eta = alpha + X * beta + combined * sigma;                  // compute eta and exponentiate into mu                   
  vector[N] rr_mu = exp(eta);                                           // output the neighbourhood-specific relative risks in mu
  real rr_beta = exp(beta);                                             // output the risk ratios for each coefficient
  real rr_alpha = exp(alpha);                                           // output the risk ratios for the intercept
}
//end script

