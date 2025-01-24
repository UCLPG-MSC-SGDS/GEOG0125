
data {
	int<lower = 0> N;              // Sample size is 1000
	real bmi[N];                   // bmi as a continuous variable of N size. bmi[N] is an array
}

parameters {
  real<lower=0> mu;              // Mean, a continuous constrained non-negative parameter
  real<lower=0> sigma;           // SD, is also a continuous constrained non-negative parameter

}

model {
	bmi ~ normal(mu, sigma);        // our likelihood function for bmi
	sigma ~ cauchy(0, 1);           // prior distribution sigma using half-cauchy
}
