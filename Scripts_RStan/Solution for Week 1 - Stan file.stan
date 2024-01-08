
data {
	int<lower = 0> Total;                      // Total at risk of metallic poisoning (50)
	int<lower = 0> Poisoned;                   // Number of people with metallic poisoning (19)
}

parameters {
	real<lower=0, upper=1> theta;             // Parameter to infer the prevalence
}

model {
	Poisoned ~ binomial(Total, theta);        // our likelihood function or observation model
	theta ~ beta(3, 5);                       // our prior distribution updated to alpha = 3 and beta = 5
}
