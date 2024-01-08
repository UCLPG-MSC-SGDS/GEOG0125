data {
	int<lower = 0> N;
	real d[N];
	real r[N];
}

parameters {
	real mu_d;
	real mu_r;
	real<lower = 0> sigma_d;
	real<lower = 0> sigma_r;
}

model{
	d ~ normal(mu_d, sigma_d);
	r ~ normal(mu_r, sigma_r);
	mu_d ~ normal(0, 10);
	mu_r ~ normal(0, 10);
	sigma_d ~ cauchy(0, 10);
	sigma_r ~ cauchy(0, 10);
}
