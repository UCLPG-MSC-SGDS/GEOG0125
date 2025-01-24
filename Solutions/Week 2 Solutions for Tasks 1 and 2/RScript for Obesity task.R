rm(list = ls())
gc()

obesity <- read.csv("Obesity and Fastfoods in MSOAs data.csv")

# RESTCAT is categorical so apply the as.factor() function
obesity$RESTCAT <- as.factor(obesity$RESTCAT)

# keep only IMDMSOA and RESTCAT to push to matrix
SelectedVariables <- obesity[,c(6,7)]
# convert to matrix
X <- model.matrix(~ 0 + IMDMSOA + RESTCAT, data = SelectedVariables)
# see conversion
View(X)
# drop RESTCAT0 = No outlets in MSOA. 
X <- X[,-2]

# declare the dependent variable 
outcome <- obesity$OBESE
# denominators for proportions
denominators <- obesity$TOTAL

dataset <- list(N=length(outcome), k=ncol(X), X=X, obesity=outcome, total=denominators)

# run stan model
results = stan(file = "Stan script for obesity task.stan", data = dataset, chains = 3, iter = 3000, warmup = 500, thin = 10)
print(results, pars = c("beta"))