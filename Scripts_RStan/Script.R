
setwd("/Volumes/Anwar-HHD/GEOG0125/Github/2022_2023/datasets")

# extend timeout
options(timeout=100)

# install package with its dependency
install.packages("rstan", repos="https://cloud.r-project.org", dependencies = TRUE)

# active library
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

dataset <- list(N=13, p=4)

# icar_poisson_model = stan_model("icar_poisson_model.stan")
predicting.passes = stan("/Volumes/Anwar-HHD/GEOG0125/Github/2022_2023/Scripts_RStan/Predicting a proportion.stan", data=dataset, iter=3000, chains=3, verbose = FALSE)

summary(predicting.passes)

print(predicting.passes, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))

# https://www.r-bloggers.com/2019/01/an-introduction-to-stan-with-r/

theta.pass_draws <- extract(predicting.passes, 'theta')[[1]] 
hist(theta.pass_draws, xlim = c(0,0.8), ylim = c(0,800), 
		 main= "Posterior samples", ylab = "Posterior Density", xlab = expression(paste("Probability of Parameter: ", theta, " [%]")))


# Calculating posterior mean (estimator)
mean(theta.pass_draws)

# Calculating posterior intervals
quantile(theta.pass_draws, probs=c(0.025, 0.975))

# Creating the Sequence
xaxis = seq(0, 1, by = 0.05)

# Plotting the beta density
plot(xaxis, dbeta(xaxis, 5, 10), xlab=expression(paste("Probability of Parameter: ", theta, " [%]"),
		 ylab = "Density", type = "l",
		 col = "Red"))
lines(xaxis, dbeta(xaxis, 5, 10))

plot(xaxis, dbinom(xaxis, 13, 4), xlab=expression(paste("Probability of Parameter: ", theta, " [%]"),
	ylab = "Density", type = "l",
	col = "Red"))
lines(classes, rel.cumulative.freq0)


dataset.updated <- list(N=26, p=9)
predicting.passes.updated = stan("/Volumes/Anwar-HHD/GEOG0125/Github/2022_2023/Scripts_RStan/Predicting a proportion.stan", data=dataset.updated, iter=3000, chains=3, verbose = FALSE)

print(predicting.passes.updated, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))

theta.pass_draws.new <- extract(predicting.passes.updated, 'theta')[[1]] 
hist(theta.pass_draws.new, xlim = c(0,1), 
		 main= "Posterior samples [Updated]", ylab = "Posterior Density", xlab = expression(paste("Probability of Parameter: ", theta, " [%]")))


# Calculating posterior mean (estimator)
mean(theta.pass_draws.new)

# Calculating posterior intervals
quantile(theta.pass_draws.new, probs=c(0.025, 0.975))

# Creating the Sequence
xaxis = seq(0, 1, by = 0.05)

