
data {
	int N;							// number of observations
	real X[N];					// data values defined in vector x 
}

parameters {
	real mu;						// estimate the mean defined as mu
	real sigma;					// estimate the standard deviation defined as sigma
}

model {
	X ~ normal(mu, sigma);
}
