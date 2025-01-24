data {
	int<lower = 0> N;                              // Number of students
	int<lower = 0> CL;                             // Total number of classrooms
	int<lower = 0, upper = CL> ClassroomID[N];     // Aligning the classroom ID number to the student ID
	int<lower = 0> k;                              // Specify total number of independent variables to be used in model
	real<lower = 0> ActiveTime[N];                 // First independent variable
	real<lower = 0> Supportive[N];                 // Second independent variable
	real<lower = 0> MathScore[N];                  // Outcome or dependent variable
}

parameters {
	real gamma00;                                  // Declare the fixed part (intercept)
	vector[k] beta;                                // Declare the fixed part (coefficients for ActiveTime and Supportive)
	vector[CL] u;                                  // Declare the random effects u0,1 u0,2, u0,3 and u0,4
	real<lower = 0> sigma_error;                   // Declare the random part (level-1)
	real<lower = 0> group_error;                   // Declare the random part (level-2)
}

model {
	real mu;
	u ~ normal(0, group_error);
	gamma00 ~ normal(0, 20);
	beta ~ normal(0, 20);
	
	for (i in 1:N) {
		mu = gamma00 + u[ClassroomID[i]] + beta[1]*ActiveTime[i] + beta[2]*Supportive[i];
		MathScore[i] ~ normal(mu, sigma_error);
	}
}
