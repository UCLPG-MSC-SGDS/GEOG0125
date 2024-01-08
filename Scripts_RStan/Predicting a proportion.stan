data {
	int<lower = 0> N;
	int<lower = 0> p;
}

parameters {
	real<lower=0, upper=1> theta;
}

model {
	p ~ binomial(N, theta); // our likelihood function or observation model
	theta ~ beta(5, 10); // our prior distribution alpha = 1 and beta = 1
}

