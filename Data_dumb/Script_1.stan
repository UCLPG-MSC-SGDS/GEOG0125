
data {
	int<lower=1> N;
	int<lower=1> Country;
	int<lower=1, upper=Country> CountryID[N];
	int<lower=0> Cholera[N];
	real Water[N];
	real Sanitation[N];
	real GDP[N];
	real Rainfall[N];
	real Temperature[N];
	real Log_Population[N];
	real Overdispersion_Parameter; 
}

parameters{
	real gamma00;
	real gamma01;
	real gamma02;
	real beta3;
	real beta4;
	real beta5;
	real<lower=0> group_intercept_sd;
	real<lower=0> group_slope_water_sd;
	real<lower=0> group_slope_sanitation_sd;
	real random_intercept[Country];
	real random_slope_water[Country];
	real random_slope_sanitation[Country];
	real phi;
}

transformed parameters {
	real beta00[Country];
	real beta01[Country];
	real beta02[Country];
	
	for (j in 1:Country) {
		beta00[j] = gamma00 + random_intercept[j];
		beta01[j] = gamma01 + random_slope_water[j];
		beta02[j] = gamma02 + random_slope_sanitation[j];
	}
}

model {
	gamma00 ~ normal(0, 1);
	gamma01 ~ normal(0, 1);
	gamma02 ~ normal(0, 1);
	beta3 ~ normal(0, 1);
	beta4 ~ normal(0, 1);
	beta5 ~ normal(0, 1);
	
	random_intercept ~ normal(0, group_intercept_sd);
	random_slope_water ~ normal(0, group_slope_water_sd);
	random_slope_sanitation ~ normal(0, group_slope_sanitation_sd);
	
	group_intercept_sd ~ cauchy(0, 0.5);
  group_slope_water_sd ~ cauchy(0, 0.5);
  group_slope_sanitation_sd ~ cauchy(0, 0.5);
  
  phi ~ cauchy(0, Overdispersion_Parameter);
  
  for (i in 1:N) {
  	Cholera[i] ~ neg_binomial_2_log(beta00[CountryID[i]] + beta01[CountryID[i]]*Water[i] + beta02[CountryID[i]]*Sanitation[i] + beta3*GDP[i] + beta4*Rainfall[i] + beta5*Temperature[i] + Log_Population[i], phi);
  }
}

generated quantities {
	// report the coefficients as relative risk ratios
	real gamma00_RR;
  real gamma01_RR;
  real gamma02_RR;
    
  gamma00_RR = exp(gamma00);
  gamma01_RR = exp(gamma01);
  gamma02_RR = exp(gamma02);
    
  real beta3_RR;
  real beta4_RR;
  real beta5_RR;
    
  beta3_RR = exp(beta3);
  beta4_RR = exp(beta4);
  beta5_RR = exp(beta5);
		
	// report the varying slopes as relative risk ratios
  vector[13] beta01_RR;
  beta01_RR[1] = exp(beta01[1]);
  beta01_RR[2] = exp(beta01[2]);
  beta01_RR[3] = exp(beta01[3]);
  beta01_RR[4] = exp(beta01[4]);
  beta01_RR[5] = exp(beta01[5]);
  beta01_RR[6] = exp(beta01[6]);
  beta01_RR[7] = exp(beta01[7]);
  beta01_RR[8] = exp(beta01[8]);
  beta01_RR[9] = exp(beta01[9]);
  beta01_RR[10] = exp(beta01[10]);
  beta01_RR[11] = exp(beta01[11]);
  beta01_RR[12] = exp(beta01[12]);
  beta01_RR[13] = exp(beta01[13]);
    
  vector[13] beta02_RR;
  beta02_RR[1] = exp(beta02[1]);
  beta02_RR[2] = exp(beta02[2]);
  beta02_RR[3] = exp(beta02[3]);
  beta02_RR[4] = exp(beta02[4]);
  beta02_RR[5] = exp(beta02[5]);
  beta02_RR[6] = exp(beta02[6]);
  beta02_RR[7] = exp(beta02[7]);
  beta02_RR[8] = exp(beta02[8]);
  beta02_RR[9] = exp(beta02[9]);
  beta02_RR[10] = exp(beta02[10]);
  beta02_RR[11] = exp(beta02[11]);
  beta02_RR[12] = exp(beta02[12]);
  beta02_RR[13] = exp(beta02[13]);
}
// end script


