# set-up
library("rstan")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

setwd("/Volumes/Anwar-HHD/GEOG0125/Github/2022_2023/Scripts_RStan")

# create dataset
task.dataset <- list(Total=50, Poisoned=19)

# run stan code 
predictions.poisoning = stan("Solution for Week 1 - Stan file.stan", data=task.dataset, iter=3000, chains=3, verbose = FALSE)
# show summary table
print(predictions.poisoning, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))

# extracting the samples (it should be 4500)
theta.draws <- extract(predictions.poisoning, 'theta')[[1]] 

# create a graph i.e., histogram
hist(theta.draws, xlim = c(0,1), 
	main= "Posterior samples", ylab = "Posterior Density", xlab = expression(paste("Predicted Prevalence: ", theta, " [%]")))

# Calculating posterior mean (estimator of the prevalence of metallic poisoning)
mean(theta.draws)
# Calculating posterior intervals for prevalence
quantile(theta.draws, probs=c(0.025, 0.975))

# The predicted prevalence is 0.3787 (37.87%) with 95% credibility of 0.260 and 0.509 
# We can formally write this as θ = 37.87% (95% CrI: 26.01-50.95%).