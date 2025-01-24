
library('rstan')
library("loo")
library("MASS")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

setwd("/Users/anwarmusah/Documents/Websites/GEOG0125/Dataset for Week 2")

burglaryDataset <- read.csv("Street Burglary Data in Nigeria.csv")
names(burglaryDataset)

# see lowest count
min(burglaryDataset$burglary)
# see highest count 
max(burglaryDataset$burglary)

# visual distribution
hist(burglaryDataset$burglary, breaks=20, xlim = c(0, 25), ylim = c(0, 600),
	xlab = "Report number of burglaries on a street segment",
	ylab = "Frequency",
	main = "Distribution of Burglary counts")

# let's start by fitting only continuous independent variable
# distance
# connectivity

#########################################################
# get the dispersion parameter
library("MASS")
# Fit negative binomial model
nb_model <- glm.nb(burglary ~ 1, data = burglaryDataset)
# Extract theta
theta <- nb_model$theta
#########################################################

stan_dataset_model1 <- list(N = nrow(burglaryDataset),
	burg = burglaryDataset$burglary, 
	dist = burglaryDataset$distance,
	conn = burglaryDataset$connectivity,
	offset = log(burglaryDataset$totalhouses),
	phi = 0.31614)

setwd("/Users/anwarmusah/Documents/Websites/GEOG0125")

# the directory needs to be set to where you saved the dataset and Stan script
crr.negbin.model1 = stan("Week_2_Cont_Model.stan", data=stan_dataset_model1, iter=3000, chains=6, verbose = FALSE)

print(crr.negbin.model1, pars = c("alpha", "beta", "baselineCrimeRR", "CrimeRR"), probs = c(0.025, 0.975))

conn_draws <- extract(crr.negbin.model1, 'CrimeRR[2]')[[1]]
mean(conn_draws > 1.07)

# extracting leave-one-out results from interaction model
log_lik_with_cont <- extract_log_lik(crr.negbin.model1, merge_chains = FALSE)
r_eff_with_cont <- relative_eff(log_lik_with_cont)
loo_cont <- loo(log_lik_with_cont, r_eff_with_cont, cores = 6)
print(loo_cont)

# Include a categorical variable
table(burglaryDataset$choiceq)
table(burglaryDataset$integq)

# Create dummy variables for choice
burglaryDataset$choicecat2 <- ifelse(burglaryDataset$choiceq == 2, 1, 0)
burglaryDataset$choicecat3 <- ifelse(burglaryDataset$choiceq == 3, 1, 0)
burglaryDataset$choicecat4 <- ifelse(burglaryDataset$choiceq == 4, 1, 0)

# Create dummy variables for integration
burglaryDataset$integ2 <- ifelse(burglaryDataset$integq == 2, 1, 0)
burglaryDataset$integ3 <- ifelse(burglaryDataset$integq == 3, 1, 0)
burglaryDataset$integ4 <- ifelse(burglaryDataset$integq == 4, 1, 0)

stan_dataset_model2 <- list(N = nrow(burglaryDataset),
	burg = burglaryDataset$burglary, 
	dist = burglaryDataset$distance,
	conn = burglaryDataset$connectivity,
	chcat2 = burglaryDataset$choicecat2,
	chcat3 = burglaryDataset$choicecat3,
	chcat4 = burglaryDataset$choicecat4,
	intcat2 = burglaryDataset$integ2,
	intcat3 = burglaryDataset$integ3,
	intcat4 = burglaryDataset$integ4,
	offset = log(burglaryDataset$totalhouses),
	phi = 0.31614)

# the directory needs to be set to where you saved the dataset and Stan script
crr.negbin.model2 = stan("Week_2_Cat_Model.stan", data=stan_dataset_model2, iter=3000, chains=6, verbose = FALSE)

print(crr.negbin.model2, pars = c("baselineCrimeRR", "betaRR", "chcqRR", "intqRR"), probs = c(0.025, 0.975))

# extracting leave-one-out results from interaction model
log_lik_with_cat <- extract_log_lik(crr.negbin.model2, merge_chains = FALSE)
r_eff_with_cat <- relative_eff(log_lik_with_cat)
loo_cat <- loo(log_lik_with_cat, r_eff_with_cat, cores = 6)

print(loo_cont)
print(loo_cat)

