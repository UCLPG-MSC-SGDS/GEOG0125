data {
  int<lower=1> N;                                  // Number of observations
  int<lower=1> Country;                            // Number of countries
  array[N] int<lower=1, upper=Country> CountryID;  // Country IDs 
  array[N] int<lower=0> Cholera;                   // Cholera cases
  array[N] real Water;                             // Water access variable
  array[N] real Sanitation;                        // Sanitation variable
  array[N] real GDP;                               // GDP variable
	array[N] real Rainfall;                          // Rainfall variable         
	array[N] real Temperature;                       // Temperature variable
  array[N] real Log_Population;                    // Logged Population variable used as offset
  real<lower=0> Overdispersion_Parameter;          // Over-dispersion set to 2 as the initiate value
}

parameters {
  real gamma00;                                    // Overall intercept
  real gamma01;                                    // Overall effect of Water
  real gamma02;                                    // Overall effect of Sanitation
  real beta3;                                      // overall fixed effects relationship with GDP
	real beta4;                                      // overall fixed effects relationship with rainfall
	real beta5;                                      // overall fixed effects relationship with temperature
  array[Country] real random_intercept;            // Country-specific random intercepts
  array[Country] real random_slope_water;          // Country-specific random slopes for Water
  array[Country] real random_slope_sanitation;     // Country-specific random slopes for Sanitation
  real<lower=0> group_intercept_sd;                // SD of random intercepts
  real<lower=0> group_slope_water_sd;              // SD of random slopes for Water
  real<lower=0> group_slope_sanitation_sd;         // SD of random slopes for Sanitation
  real<lower=0> phi;                               // Use phi to create a distribution around the Overdispersion_Parameter
}
  
transformed parameters {
  array[Country] real beta00;
  array[Country] real beta01;
  array[Country] real beta02;

  for (j in 1:Country) {
    beta00[j] = gamma00 + random_intercept[j];           // Random intercept per country - overall risks of cholera varying by country
    beta01[j] = gamma01 + random_slope_water[j];         // Random slope for Water - overall risks cholera with `Water` varying by country
    beta02[j] = gamma02 + random_slope_sanitation[j];    // Random slope for Sanitation - overall risks cholera with `Sanitation` varying by country
  }
}

model {
  // Priors for fixed effects
  gamma00 ~ normal(0, 1);
  gamma01 ~ normal(0, 1);
  gamma02 ~ normal(0, 1);
  beta3 ~ normal(0, 1);
  beta4 ~ normal(0, 1);
  beta5 ~ normal(0, 1);

  // Priors for random effects
  random_intercept ~ normal(0, group_intercept_sd);
  random_slope_water ~ normal(0, group_slope_water_sd);
  random_slope_sanitation ~ normal(0, group_slope_sanitation_sd);
  
  // Priors for standard deviations of random effects
  group_intercept_sd ~ cauchy(0, 0.5);
  group_slope_water_sd ~ cauchy(0, 0.5);
  group_slope_sanitation_sd ~ cauchy(0, 0.5);

  // Prior for overdispersion parameter
  phi ~ cauchy(0, Overdispersion_Parameter);

  // Likelihood: Negative Binomial Poisson Regression
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
  array[Country] real beta00_RR;
  array[Country] real beta01_RR;
  array[Country] real beta02_RR;
  
  beta00_RR = exp(beta00);
  beta01_RR = exp(beta01);
  beta02_RR = exp(beta02);
}
// end script

