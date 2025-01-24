rm(list = ls())
gc()

setwd("/Users/anwarmusah/Desktop/Projects/Workshop/RStan-CPD/Datasets/Day 3")

# load up the data
preg_dataset <- read.csv("Pregnancies and hospital data.csv")

stan.preg.dataset <- list(N=nrow(preg_dataset), 
	mother=max(unique(preg_dataset$motherID)),
	MotherID=as.integer(preg_dataset$motherID),
	k=2,
	logincome=preg_dataset$logincome,
	distance=preg_dataset$distance,
	dropout=preg_dataset$dropout,
	births=preg_dataset$births
)


# Start the clock
ptm <- proc.time()
# compile linear regression model for now
bayesian.hierarchical.model = stan("Preg_stan_script.stan", data=stan.preg.dataset, iter=10000, chains=6, verbose = FALSE)
# Stop the clock
proc.time() - ptm

print(bayesian.hierarchical.model, pars = c("gamma00","gamma01","beta", "sigma_error", "group_error"))