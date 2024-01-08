// Example of using stan to perform a multivariable linear regression model
// Assessing the association between average LSOA house price with IMD, income and PTAL scores

data {
	int N;                               // defining the number of observations
	int K;                               // defining the number of parameters for K variables
	
	real<lower=0> AVEINCOME[N];          // specification for average income variable
	real<lower=0> IMDSCORE[N];           // specification for IMD variable
	real<lower=0> PTAINDEX[N];           // specification for PTAL index variable
	real<lower=0> AVEPRICE[N];           // specification for AVEPRICE to be used as outcome in model
}

parameters {
	vector[K] beta;                      // defining the parameters to estimate
	real<lower=0> sigma;                 // defining the standard deviation as positive real number
}

model {
	// define mu i.e., predicted outcome
	real mu[N];
	// model formulation 
	for(i in 1:N) {
	mu[i] = beta[1] + beta[2]*AVEINCOME[i] + beta[3]*IMDSCORE[i] + beta[4]*PTAINDEX[i];
	AVEPRICE[i] ~ normal(mu[i], sigma);  // define the likelihood function to compute Bayesian inference
	}
}
