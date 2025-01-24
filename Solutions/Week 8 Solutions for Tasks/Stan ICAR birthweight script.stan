// Stan script: Spatial ICAR Poisson model

data {
  int<lower=0> N;                                                        // number of spatial units or neighbourhoods
  int<lower=0> N_edges;                                                  // number of edges connecting adjacent areas using Queen's contiguity
  array[N_edges] int<lower=1, upper=N> node1;                            // list of index areas showing which spatial units are neighbours
  array[N_edges] int<lower=1, upper=N> node2;                            // list of neighbouring areas showing the connection to index spatial unit
  array[N] int<lower=0> Y;                                               // dependent variable
  vector<lower=0>[N] Offset;                                             // offset variable
  real<lower=0> factor;                                                  // scaling factor the variance of the spatial effects
}

transformed data {
	vector[N] log_Offset = log(Offset);
}

parameters {
  real alpha;                                                            // intercept
  real<lower=0> sigma;                                                   // overall standard deviation
  real<lower=0, upper=1> rho;                                            // proportion unstructured vs. spatially structured variance
  vector[N] theta;                                                       // unstructure random effects (heterogeneous)
  vector[N] phi;                                                         // spatial random effects
}

transformed parameters {
  vector[N] combined;                                                    // combined random effect i.e., unstructure and structured
  combined = sqrt(1 - rho) * theta + sqrt(rho/factor) * phi;             // formulation for the combined random effect i.e., unstructure and structured
}


model {
  // likelihood function: multivariable Poisson ICAR regression model
  Y ~ poisson_log(log_Offset + alpha + combined * sigma);
  // setting priors
  alpha ~ normal(0.0, 1.0);                                             // prior for alpha: weakly informative
  theta ~ normal(0.0, 1.0);                                             // prior for theta: weakly informative
  sigma ~ normal(0.0, 1.0);                                             // prior for sigma: weakly informative
  rho ~ beta(0.5, 0.5);                                                 // prior for rho: pulled for literature
  target += -0.5 * dot_self(phi[node1] - phi[node2]);
  sum(phi) ~ normal(0, 0.001 * N); 
}

generated quantities {
  real logit_rho = log(rho / (1.0 - rho));
  vector[N] eta = alpha + combined * sigma;                             // compute eta and exponentiate into mu                   
  vector[N] rr_mu = exp(eta);                                           // output the neighbourhood-specific relative risks in mu
  real rr_alpha = exp(alpha);                                           // output the risk ratios for the intercept
}
//end script
