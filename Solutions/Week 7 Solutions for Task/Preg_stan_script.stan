
data {
	int<lower=0> N;
	int<lower=0> mother;
	int<lower=0, upper=mother> MotherID[N];
	int<lower=0> k;
	int<lower=0, upper=1> births[N];
	real<lower=0> logincome[N];
	real<lower=0> distance[N];
	int<lower=0, upper=1> dropout[N];
}

parameters {
	real gamma00;
	real gamma01;
	vector[k] beta;
	real u[mother];
	real<lower=0> sigma_error;
	real<lower=0> group_error;
}

transformed parameters {
	real beta00[mother];
	
	for (j in 1:mother) {
		beta00[j] = gamma00 + u[j];
	}
	
	real beta01[mother];
	
	for (j in 1:mother){
		beta01[j] = gamma01 + u[j];
	}
}

model {
	u ~ normal(0, group_error);
	gamma00 ~ normal(0, 20);
	gamma01 ~ normal(0, 20);
	beta ~ normal(0, 20);
	group_error ~ gamma(5, 5);
	sigma_error ~ cauchy(0, 10);
	
	for (i in 1:N){
		births[i] ~ bernoulli(inv_logit(beta00[MotherID[i]] + beta[1]*logincome[i] + beta[2]*distance[i] + beta01[MotherID[i]]*dropout[i]));
	}
}
