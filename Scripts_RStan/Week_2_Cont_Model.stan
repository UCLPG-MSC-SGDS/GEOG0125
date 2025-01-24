
data {
	int<lower=0> N;                    // declare the overall number of data points to be passed into model
	int<lower=0> burg[N];              // define as an array and specify it as an integer for counts
	vector[N] dist;                    // continuous variable
	vector[N] conn;                    // continuous variable
	vector[N] offset;                  // offset variable for the denominators (total households on a stree segment)
	real<lower=0> phi;                 // fixed value to account for overdispersion in the crime counts
}

parameters {
	real alpha;
	vector[2] beta;
}

model {
	// prior specification for our parameters
	alpha ~ normal(0, 1);               
	beta[1] ~ normal(0, 1);
	beta[2] ~ normal(0, 1);
	
	// likelihood function i.e., statistical model
	for (i in 1:N) {
		burg[i] ~ neg_binomial_2_log(alpha + beta[1]*dist[i] + beta[2]*conn[i] + offset[i], phi);
	}
}

generated quantities {
	// report crime risk ratios
	real baselineCrimeRR;
	vector[2] CrimeRR;
	baselineCrimeRR = exp(alpha);
	CrimeRR = exp(beta);
	
	// model validation and comparison
	vector[N] log_lik;                 
	for (i in 1:N) {
		log_lik[i] = neg_binomial_2_log_lpmf(burg[i] | alpha + beta[1]*dist[i] + beta[2]*conn[i] + offset[i], phi);
	}
}

