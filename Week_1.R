
library('rstan')

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# create list object and call it dataset
dataset <- list(N = 13, p = 4)

# the directory needs to be set to where you save the datasets and Stan script
prediction.passes = stan("Week_1.stan", data=dataset, iter=3000, chains=3, verbose = FALSE)

print(prediction.passes, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))

# extracting the samples (it should be 4500)
theta.pass_draws <- extract(prediction.passes, 'theta')[[1]]
# create a graph i.e., histogram
hist(theta.pass_draws, xlim = c(0,0.8), ylim = c(0,800), 
	main= "Posterior samples", ylab = "Posterior Density (Plausibility)", xlab = expression(paste("Pass rate > 70%: ", theta, " [%]")))

plot(density(theta.pass_draws), main = "", xlab = expression(paste("Pass rate > 70%: ", theta, " [%]")) , ylab = "Posterior Density (Plausibility)")

# Calculating posterior mean (estimator)
mean(theta.pass_draws)

# Calculating posterior intervals
quantile(theta.pass_draws, probs=c(0.025, 0.975))

# what is the probability observing a pass rate for distinction 70%+?
mean(theta.pass_draws >= 0.333)
#[1] 0.4824444

# # what is the probability observing a 33% pass rate for distinction 70%+?
mean(theta.pass_draws >= 0.50)
#[1] 0.09333333

# new data for 2022/23 cohort
dataset.updated <- list(N=26, p=9)

# model for updated results based on new data
prediction.passes.updated = stan(fit=prediction.passes, data=dataset.updated, iter=3000, chains=3, verbose = FALSE)	
print(prediction.passes.updated, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))


# extracting the samples (it should be 4500)
theta.pass_draws.new <- extract(prediction.passes.updated, 'theta')[[1]] 

# create a graph i.e., histogram
hist(theta.pass_draws.new, xlim = c(0,1), 
	main= "Posterior samples [Updated]", ylab = "Posterior Density (Plausibility)", xlab = expression(paste("Pass rate (Distinction) Parameter: ", theta, " [%]")))

# create a graph i.e., density
plot(density(theta.pass_draws.new), main = "", xlab = expression(paste("Pass rate > 70%: ", theta, " [%]")) , ylab = "Posterior Density (Plausibility)")


# Calculating posterior mean (estimator)
mean(theta.pass_draws.new)

#[1] 0.3587437

# Calculating posterior intervals
quantile(theta.pass_draws.new, probs=c(0.025, 0.975))
#2.5%     97.5% 
#0.2027673 0.4935101