

data {
	int<lower=0> N;                    // declare the overall number of data points to be passed into model
	int<lower=0> burg[N];              // define as an array and specify it as an integer for counts
	vector[N] dist;                    // continuous variable
	vector[N] conn;                    // continuous variable
	int<lower=0, upper=1> chcat2[N];
	int<lower=0, upper=1> chcat3[N];
	int<lower=0, upper=1> chcat4[N];
	int<lower=0, upper=1> intcat2[N];
	int<lower=0, upper=1> intcat3[N];
	int<lower=0, upper=1> intcat4[N];
	vector[N] offset;                  // offset variable for the denominators (total households on a stree segment)
	real<lower=0> phi;                 // fixed value to account for overdispersion in the crime counts
}

parameters {
	real alpha;
	vector[2] beta;
	vector[3] chcq;
	vector[3] intq;
}

model {
	// prior specification for our parameters
	alpha ~ normal(0, 1);               
	beta ~ normal(0, 1);
	chcq  ~ normal(0, 1);
	intq  ~ normal(0, 1);
	
	// likelihood function i.e., statistical model
	for (i in 1:N) {
		burg[i] ~ neg_binomial_2_log(alpha + beta[1]*dist[i] + beta[2]*conn[i] + chcq[1]*chcat2[i] + chcq[2]*chcat3[i] + chcq[3]*chcat4[i] + intq[1]*intcat2[i] + intq[2]*intcat3[i] + intq[3]*intcat4[i] + offset[i], phi);
	}
}

generated quantities {
	// report crime risk ratios
	real baselineCrimeRR;
	vector[2] betaRR;
	vector[3] chcqRR;
	vector[3] intqRR;
	baselineCrimeRR = exp(alpha);
	betaRR = exp(beta);
	chcqRR = exp(chcq);
	intqRR = exp(intq);
	
	// model validation and comparison
	vector[N] log_lik;                 
	for (i in 1:N) {
		log_lik[i] = neg_binomial_2_log_lpmf(burg[i] | alpha + beta[1]*dist[i] + beta[2]*conn[i] + chcq[1]*chcat2[i] + chcq[2]*chcat3[i] + chcq[3]*chcat4[i] + intq[1]*intcat2[i] + intq[2]*intcat3[i] + intq[3]*intcat4[i] + offset[i], phi);
	}
}
// end script


