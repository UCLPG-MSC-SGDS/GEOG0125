functions {
	real icar_normal_lpdf(vector phi, int N, array[] int node1, array[] int node2) {
		return -0.5 * dot_self(phi[node1] - phi[node2]);
	}
}
data {
	int<lower=0> N;
	int<lower=0> N_edges;
	array[N_edges] int<lower=1, upper=N> node1;
	array[N_edges] int<lower=1, upper=N> node2;
	array[N] int<lower=0> y;
	vector<lower=0>[N] E;
}
transformed data {
	vector[N] log_exposure = log(E);
}
parameters {
	real alpha;
	real<lower=0> sigma;
	vector[N] phi;
}
model {
	phi ~ icar_normal(N, node1, node2);
	y ~ poisson_log(log_exposure + alpha + phi*sigma);
	alpha ~ normal(0.0, 1.0);
	sigma ~ normal(0.0, 1.0);
	sum(phi) ~ normal(0, 0.001*N);
}
generated quantities {
	vector[N] eta = alpha + phi;
	vector[N] mu = exp(eta);
}
