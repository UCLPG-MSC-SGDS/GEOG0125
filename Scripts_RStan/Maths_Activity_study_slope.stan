data {
	int<lower = 0> N;
	int<lower = 0> CL;
	int<lower = 0, upper = CL> ClassroomID[N];
	int<lower = 0> k;
	matrix[N, k] X; 
	real<lower = 0> MathScore[N];
}

parameters {
	vector[k] gamma[N];                          // individual classroom intercept as well as slope-specific coefficients
	vector<lower=0>[k] group_error;              // random effect part of the model
	vector[k] beta;                              // level-1 intercept and coefficients
	corr_matrix[k] omega;                        // correlation matrix for the get interclass correlation
	real<lower = 0> sigma_error;                 // random effect part of the model on level-1
}

model {
	vector[N] mu;
	omega ~ lkj_corr(2);
	group_error ~ exponential(1);
	sigma_error ~ exponential(1); 
	beta ~ normal(0, 20);
	gamma ~ multi_normal(beta, quad_form_diag(omega, group_error));
	
	for (i in 1:N) {
		mu = X[I] * (gamma[ClassroomID[N]]);
		MathScore[i] ~ normal(mu, sigma_error);
	}
}

