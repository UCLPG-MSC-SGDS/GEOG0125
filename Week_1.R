
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

We can ask what is the probability

```{r}
# calculating the exceedance probability
# what is the probability that income being above 50000?
mean(theta.pass_draws >= 0.333)
```

