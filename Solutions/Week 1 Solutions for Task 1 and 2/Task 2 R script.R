# Task 2
# set-up
library("rstan")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# create dataset
bmi <- rnorm(1000, mean=23, 1.2)
task2.dataset <- list(N=1000, bmi=bmi)

# run stan code 
predictions.bmi = stan("Task 2 Stan script.stan", data=task2.dataset, iter=3000, chains=3, verbose = FALSE)

# show summary table
print(predictions.bmi, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))